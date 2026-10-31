package scalapsihost.platform

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
  /**
   * Whether the calls the host answers are counted too, not only unsupported calls and logged errors: the adapter
   * surface test needs them. A server does not, and counting each of the platform's calls cost a tenth of its CPU.
   */
  @volatile var countsAnswered = false

  /** Records one adapter call under its diagnostic name. */
  def hit(name: String): Unit = counts.computeIfAbsent(name, _ => new AtomicInteger()).incrementAndGet()

  /** A call the host answered from its tables; its name is only made when such calls are counted. */
  def answered(name: => String): Unit = if (countsAnswered) hit(name)

  /** Records and throws for a call outside the host's explicit adapter surface. */
  def unsupported[T](name: String): T = {
    hit("UNSUPPORTED " + name)
    throw new UnsupportedOperationException("host adapter does not support: " + name)
  }

  /** The adapter-call counts, sorted by name for stable status output and tests. */
  def snapshot: Seq[(String, Int)] = counts.asScala.toSeq.map((k, v) => (k, v.get)).sortBy(_._1)
}

/**
 * The platform's and plugin's `Logger`, writing to [[HostLog]]: as in the IDE, `LOG.error` reports and execution
 * continues (the platform's `DefaultLogger` is the unit-test logger and throws instead). Each logged error is recorded
 * in [[CallLog]], so a run that logs one shows it in the adapter surface. Unsupported adapter calls are recorded
 * before they throw, so a caller that catches and logs one does not hide it. Their info and debug messages are not
 * kept (the IDE's default log level).
 */
final class HostLogger(category: String) extends com.intellij.openapi.diagnostic.DefaultLogger(category) {
  override def error(message: String, t: Throwable, details: String*): Unit = {
    CallLog.hit(s"LOGGED ERROR $category")
    HostLog.error(s"[$category] $message", t)
  }

  override def warn(message: String, t: Throwable): Unit = HostLog.warn(s"[$category] $message", t)
}

// What an answer table gives for a call it has no case for. One lookup (applyOrElse) instead of two (isDefinedAt,
// then apply): tables answer the platform's most frequent calls.
private val NotAnswered = new AnyRef
private val Unanswered: Any => AnyRef = _ => NotAnswered

/**
 * A fixed-table proxy: identity methods use the proxy itself, declared answers take precedence over interface
 * defaults, and unhandled calls are recorded by [[CallLog]] before throwing `UnsupportedOperationException`.
 */
def tableProxy[T](iface: Class[T], label: String)(answer: PartialFunction[(String, Array[AnyRef]), AnyRef]): T = {
  val handler: InvocationHandler = (proxy: AnyRef, m: Method, args0: Array[AnyRef]) => {
    val args = if (args0 == null) Array.empty[AnyRef] else args0
    m.getName match {
      case "hashCode" if args.isEmpty => Integer.valueOf(System.identityHashCode(proxy))
      case "equals" if args.length == 1 => java.lang.Boolean.valueOf(proxy eq args(0))
      case "toString" if args.isEmpty => s"Host$label"
      case name =>
        val answered = answer.applyOrElse((name, args), Unanswered)
        if (answered ne NotAnswered) { CallLog.answered(s"$label.$name"); answered }
        else if (m.isDefault) { CallLog.answered(s"$label.$name (default)"); InvocationHandler.invokeDefault(proxy, m, args*) }
        else CallLog.unsupported(s"$label.$name")
    }
  }
  iface.cast(Proxy.newProxyInstance(iface.getClassLoader, Array[Class[?]](iface), handler))
}

/** Answer table for generated Host_* subclasses (see tools/Gen.java); unanswered abstract methods fail loudly. */
def dispatch(answer: PartialFunction[(String, Array[AnyRef]), AnyRef]): Dispatch =
  (method: String, args0: Array[AnyRef]) => {
    val args = if (args0 == null) Array.empty[AnyRef] else args0
    val answered = answer.applyOrElse((method, args), Unanswered)
    if (answered ne NotAnswered) { CallLog.answered(method); answered }
    else CallLog.unsupported(method)
  }

/** The host has no annotation inference engine; declared annotations remain in PSI. */
private[scalapsihost] final class HostInferredAnnotationsManager extends com.intellij.codeInsight.InferredAnnotationsManager {
  override def findInferredAnnotations(owner: com.intellij.psi.PsiModifierListOwner): Array[com.intellij.psi.PsiAnnotation] = {
    CallLog.hit("InferredAnnotationsManager.findInferredAnnotations")
    com.intellij.psi.PsiAnnotation.EMPTY_ARRAY
  }
  override def findInferredAnnotation(owner: com.intellij.psi.PsiModifierListOwner, name: String): com.intellij.psi.PsiAnnotation = {
    CallLog.hit("InferredAnnotationsManager.findInferredAnnotation")
    null
  }
}

/**
 * The fixed service table of one component manager (the Application or the Project object). Only services
 * registered here exist; any other `getService` fails loudly. Instances are created on first request.
 * Concurrent first requests may invoke a factory more than once; all callers receive the instance published first.
 * Factories can request other registered services, so creation cannot use a nested `computeIfAbsent`.
 */
final class ServiceTable(label: String) {
  private val factories = new ConcurrentHashMap[Class[?], () => AnyRef]()
  private val instances = new ConcurrentHashMap[Class[?], AnyRef]()
  /** Services the host deliberately answers with null (the caller tolerates it). */
  private val nulls = ConcurrentHashMap.newKeySet[Class[?]]()

  /** Registers a lazy factory for a service class; the factory runs on its first request. */
  def register[T <: AnyRef](cls: Class[T])(make: => T): Unit = factories.put(cls, () => make)

  /** Registers a service class whose callers explicitly tolerate a `null` service. */
  def registerNull(cls: Class[?]): Unit = nulls.add(cls)

  /** Returns or lazily creates the registered service, failing if this component manager does not provide it. */
  def get(cls: Class[?]): AnyRef =
    if (nulls.contains(cls)) { CallLog.answered(s"$label.service(null) ${cls.getName}"); null }
    else {
      val make = factories.get(cls)
      if (make == null) CallLog.unsupported(s"$label.getService ${cls.getName}")
      else {
        CallLog.answered(s"$label.service ${cls.getName}")
        // Not computeIfAbsent: service constructors fetch other services, and ConcurrentHashMap forbids
        // nested updates. Concurrent first requests may build twice; one instance wins.
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

/** The platform's compact set looks up the VFS manager even for files without ids; the host keeps files directly. */
private[scalapsihost] final class HostVirtualFileSet(private var files: java.util.Set[com.intellij.openapi.vfs.VirtualFile])
    extends java.util.AbstractSet[com.intellij.openapi.vfs.VirtualFile] with com.intellij.openapi.vfs.VirtualFileSet {

  /** Creates an initially empty set that retains virtual-file identities directly. */
  def this() = this(new java.util.LinkedHashSet)

  override def iterator(): java.util.Iterator[com.intellij.openapi.vfs.VirtualFile] = files.iterator()
  override def size(): Int = files.size
  override def contains(o: Object): Boolean = files.contains(o)
  override def add(file: com.intellij.openapi.vfs.VirtualFile): Boolean = files.add(file)
  override def remove(o: Object): Boolean = files.remove(o)

  /** Freezes the set and returns this same view. */
  override def freezed(): java.util.Set[com.intellij.openapi.vfs.VirtualFile] = {
    freeze()
    this
  }

  /** Prevents further mutations by replacing the backing set with an unmodifiable view. */
  override def freeze(): Unit = files = java.util.Collections.unmodifiableSet(files)
  override def process(processor: com.intellij.util.Processor[? >: com.intellij.openapi.vfs.VirtualFile]): Boolean =
    files.asScala.forall(processor.process)
}
