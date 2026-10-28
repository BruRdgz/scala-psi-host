package scalapsihost

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

/**
 * Ledger of every adapter call. Its distinct entries are the measured adapter surface; an `UNSUPPORTED` entry
 * means the plugin or platform asked the host for something outside its tables.
 */
object CallLog {
  private val counts = new ConcurrentHashMap[String, AtomicInteger]()
  def hit(name: String): Unit = counts.computeIfAbsent(name, _ => new AtomicInteger()).incrementAndGet()
  def unsupported[T](name: String): T = {
    hit("UNSUPPORTED " + name)
    throw new UnsupportedOperationException("host adapter does not support: " + name)
  }
  def snapshot: Seq[(String, Int)] = counts.asScala.toSeq.map((k, v) => (k, v.get)).sortBy(_._1)
}

/**
 * The platform's and plugin's `Logger`: as in the IDE, `LOG.error` reports and execution continues (the platform's
 * `DefaultLogger` is the unit-test logger and throws instead). Each logged error is recorded in [[CallLog]], so a
 * run that logs one shows it in the adapter surface. Unsupported adapter calls are recorded before they throw, so a
 * caller that catches and logs one does not hide it.
 */
final class HostLogger(category: String) extends com.intellij.openapi.diagnostic.DefaultLogger(category) {
  override def error(message: String, t: Throwable, details: String*): Unit = {
    CallLog.hit(s"LOGGED ERROR $category")
    System.err.println(s"ERROR [$category] $message")
    if (t != null) t.printStackTrace()
  }
}

/** A fixed-table adapter: a Proxy that answers only what `answer` handles; default methods run as written. */
def tableProxy[T](iface: Class[T], label: String)(answer: PartialFunction[(String, Array[AnyRef]), AnyRef]): T = {
  val handler: InvocationHandler = (proxy: AnyRef, m: Method, args0: Array[AnyRef]) => {
    val args = if (args0 == null) Array.empty[AnyRef] else args0
    m.getName match {
      case "hashCode" if args.isEmpty => Integer.valueOf(System.identityHashCode(proxy))
      case "equals" if args.length == 1 => java.lang.Boolean.valueOf(proxy eq args(0))
      case "toString" if args.isEmpty => s"Host$label"
      case name =>
        val key = (name, args)
        if (answer.isDefinedAt(key)) { CallLog.hit(s"$label.$name"); answer(key) }
        else if (m.isDefault) { CallLog.hit(s"$label.$name (default)"); InvocationHandler.invokeDefault(proxy, m, args*) }
        else CallLog.unsupported(s"$label.$name")
    }
  }
  iface.cast(Proxy.newProxyInstance(iface.getClassLoader, Array[Class[?]](iface), handler))
}

/** Answer table for generated Host_* subclasses (see tools/Gen.java); unanswered abstract methods fail loudly. */
def dispatch(answer: PartialFunction[(String, Array[AnyRef]), AnyRef]): Dispatch =
  (method: String, args0: Array[AnyRef]) => {
    val args = if (args0 == null) Array.empty[AnyRef] else args0
    if (answer.isDefinedAt((method, args))) { CallLog.hit(method); answer((method, args)) }
    else CallLog.unsupported(method)
  }

/**
 * The fixed service table of one component manager (the Application or the Project object). Only services
 * registered here exist; any other `getService` fails loudly. Instances are created on first request.
 */
final class ServiceTable(label: String) {
  private val factories = new ConcurrentHashMap[Class[?], () => AnyRef]()
  private val instances = new ConcurrentHashMap[Class[?], AnyRef]()
  /** Services the host deliberately answers with null (the caller tolerates it). */
  private val nulls = ConcurrentHashMap.newKeySet[Class[?]]()

  def register[T <: AnyRef](cls: Class[T])(make: => T): Unit = factories.put(cls, () => make)
  def registerNull(cls: Class[?]): Unit = nulls.add(cls)

  def get(cls: Class[?]): AnyRef =
    if (nulls.contains(cls)) { CallLog.hit(s"$label.service(null) ${cls.getName}"); null }
    else {
      val make = factories.get(cls)
      if (make == null) CallLog.unsupported(s"$label.getService ${cls.getName}")
      else {
        CallLog.hit(s"$label.service ${cls.getName}")
        // Not computeIfAbsent: service constructors fetch other services, and ConcurrentHashMap forbids
        // nested updates. Concurrent first requests may build twice; one instance wins. Threading is phase F3.
        val existing = instances.get(cls)
        if (existing != null) existing
        else {
          val created = make()
          val raced = instances.putIfAbsent(cls, created)
          if (raced != null) raced else created
        }
      }
    }
}
