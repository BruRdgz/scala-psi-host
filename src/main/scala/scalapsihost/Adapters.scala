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

/**
 * The host's read/write lock, the IDE's read and write actions: queries read concurrently, an edit writes alone.
 * An edit first cancels the running queries (their progress indicators), as the IDE cancels background reads for a
 * pending write: the platform's and the plugin's code checks for cancellation as it works, so a query stops with
 * `ProcessCanceledException` and releases the lock. The lock is fair: queries started after a waiting edit wait
 * for it.
 */
private[scalapsihost] object HostLock {
  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock(/* fair = */ true)
  private val queries = java.util.concurrent.ConcurrentHashMap.newKeySet[com.intellij.openapi.progress.ProgressIndicator]()

  def isReading: Boolean = lock.getReadHoldCount > 0
  def isWriting: Boolean = lock.isWriteLockedByCurrentThread

  /** An editor notification can cancel reads before its queued write acquires the documents thread. */
  def cancelQueries(): Unit = queries.forEach(_.cancel())

  /** A read without its own cancellation (the platform's `runReadAction`): the lock only. */
  def readLocked[T](body: => T): T =
    if (isReading || isWriting) body
    else {
      lock.readLock.lock()
      try body
      finally lock.readLock.unlock()
    }

  /** [[readLocked]] for Java callers. */
  def readLocked[T](body: java.util.concurrent.Callable[T]): T = readLocked(body.call())

  /** A query: a read that an edit cancels. Nested in another read, it is part of that one. */
  def query[T](body: => T): T =
    if (isReading || isWriting) body
    else {
      val indicator = new com.intellij.openapi.progress.EmptyProgressIndicator
      lock.readLock.lock()
      queries.add(indicator)
      try com.intellij.openapi.progress.ProgressManager.getInstance.runProcess((() => body): com.intellij.openapi.util.Computable[T], indicator)
      finally {
        queries.remove(indicator)
        lock.readLock.unlock()
      }
    }

  def write[T](body: => T): T = {
    // A read lock cannot become a write lock: waiting for it here would wait for this thread's own read forever.
    if (isReading && !isWriting) throw new IllegalStateException("an edit cannot start inside a read")
    if (!isWriting) cancelQueries()
    lock.writeLock.lock()
    try body
    finally lock.writeLock.unlock()
  }
}

/**
 * The host's event dispatch thread: a thread without UI where work the platform schedules for the IDE's UI thread
 * (`invokeLater`, `invokeAndWait`) runs. It is declared to the platform as its EDT the way the IDE's event queue
 * declares it (`EDT.updateEdt`), so the platform's EDT checks hold there.
 */
private[scalapsihost] object HostDispatchThread {
  private val executor = java.util.concurrent.Executors.newSingleThreadExecutor { runnable =>
    val thread = new Thread(runnable, "scala-psi-host-dispatch")
    thread.setDaemon(true)
    thread
  }
  executor.submit((() => com.intellij.util.ui.EDT.updateEdt()): Runnable).get()

  def isCurrent: Boolean = com.intellij.util.ui.EDT.isCurrentThreadEdt

  def invokeLater(action: Runnable): Unit = executor.execute(action)

  def invokeAndWait(action: Runnable): Unit =
    if (isCurrent) action.run() else executor.submit(action).get()

  /** Waits until the work scheduled so far has run. */
  def drain(): Unit = invokeAndWait(() => ())
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

/** A completion request's editor state, without a UI or any insertion operations. */
private[scalapsihost] object CompletionEditor {
  def apply(document: com.intellij.openapi.editor.Document, project: com.intellij.openapi.project.Project,
            offset: Int): com.intellij.openapi.editor.Editor = {
    import com.intellij.openapi.editor.{Caret, CaretModel, Editor, SelectionModel}
    import com.intellij.openapi.util.{Key, UserDataHolderBase}
    val data = new UserDataHolderBase
    lazy val editor: Editor = tableProxy(classOf[Editor], "CompletionEditor") {
      case ("getDocument", _) => document
      case ("getProject", _) => project
      case ("getCaretModel", _) => model
      case ("getSelectionModel", _) => selection
      case ("getUserData", Array(k: Key[?])) => data.getUserData(k).asInstanceOf[AnyRef]
      case ("putUserData", Array(k: Key[?], value)) => putData(k, value); null
    }
    // Reflection erases Key's type parameter, as in the project's user-data table.
    def putData(key: Key[?], value: AnyRef): Unit = data.putUserData(key.asInstanceOf[Key[AnyRef]], value)
    lazy val caret: Caret = tableProxy(classOf[Caret], "CompletionCaret") {
      case ("getEditor", _) => editor
      case ("getOffset" | "getSelectionStart" | "getSelectionEnd", _) => Integer.valueOf(offset)
      case ("hasSelection", _) => java.lang.Boolean.FALSE
    }
    lazy val model: CaretModel = tableProxy(classOf[CaretModel], "CompletionCaretModel") {
      case ("getCurrentCaret" | "getPrimaryCaret", _) => caret
      case ("getOffset", _) => Integer.valueOf(offset)
      case ("getAllCarets", _) => java.util.List.of(caret)
    }
    lazy val selection: SelectionModel = tableProxy(classOf[SelectionModel], "CompletionSelection") {
      case ("getSelectionStart" | "getSelectionEnd", _) => Integer.valueOf(offset)
      case ("hasSelection", _) => java.lang.Boolean.FALSE
    }
    editor
  }
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
