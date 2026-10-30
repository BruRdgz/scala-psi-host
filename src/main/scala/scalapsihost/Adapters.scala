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
  /**
   * Whether the calls the host answers are counted too, not only unsupported calls and logged errors: the adapter
   * surface test needs them. A server does not, and counting each of the platform's calls cost a tenth of its CPU.
   */
  @volatile var countsAnswered = false

  def hit(name: String): Unit = counts.computeIfAbsent(name, _ => new AtomicInteger()).incrementAndGet()

  /** A call the host answered from its tables; its name is only made when such calls are counted. */
  def answered(name: => String): Unit = if (countsAnswered) hit(name)
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
 * The host's read/write lock, the IDE's read and write actions: queries and copy changes read concurrently,
 * an editor edit writes alone.
 * An edit first cancels the running queries (their progress indicators), as the IDE cancels background reads for a
 * pending write: the platform's and the plugin's code checks for cancellation as it works, so a query stops with
 * `ProcessCanceledException` and releases the lock. The lock is fair: queries started after a waiting edit wait
 * for it.
 */
private[scalapsihost] object HostLock {
  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock(/* fair = */ true)
  private val queries = java.util.concurrent.ConcurrentHashMap.newKeySet[com.intellij.openapi.progress.ProgressIndicator]()

  def isReading: Boolean = lock.getReadHoldCount > 0
  private val intent = new java.util.concurrent.locks.ReentrantLock(true)
  private val writeListeners = new java.util.concurrent.CopyOnWriteArrayList[com.intellij.openapi.application.WriteActionListener]()
  private val readListeners = new java.util.concurrent.CopyOnWriteArrayList[com.intellij.openapi.application.ReadActionListener]()
  private val intentListeners = new java.util.concurrent.CopyOnWriteArrayList[com.intellij.openapi.application.WriteIntentReadActionListener]()

  def isWriting: Boolean = lock.isWriteLockedByCurrentThread
  def hasWriteIntent: Boolean = intent.isHeldByCurrentThread

  def addWriteListener(listener: com.intellij.openapi.application.WriteActionListener,
                       parent: com.intellij.openapi.Disposable): Unit = {
    writeListeners.add(listener)
    com.intellij.openapi.util.Disposer.register(parent, (() => { writeListeners.remove(listener); () }): com.intellij.openapi.Disposable)
  }

  def addReadListener(listener: com.intellij.openapi.application.ReadActionListener,
                      parent: com.intellij.openapi.Disposable): Unit = {
    readListeners.add(listener)
    com.intellij.openapi.util.Disposer.register(parent, (() => { readListeners.remove(listener); () }): com.intellij.openapi.Disposable)
  }

  def addIntentListener(listener: com.intellij.openapi.application.WriteIntentReadActionListener,
                        parent: com.intellij.openapi.Disposable): Unit = {
    intentListeners.add(listener)
    com.intellij.openapi.util.Disposer.register(parent, (() => { intentListeners.remove(listener); () }): com.intellij.openapi.Disposable)
  }

  /** Write intent serializes potential writers while still allowing other threads to read. */
  def writeIntent[T](body: => T): T = {
    if (isReading && !hasWriteIntent) throw new IllegalStateException("write intent cannot start inside a read")
    if (hasWriteIntent) body
    else {
      val listeners = intentListeners.asScala.toVector
      val action = HostLock.getClass
      listeners.foreach(_.beforeWriteIntentReadActionStart(action))
      intent.lock()
      try {
        listeners.foreach(_.writeIntentReadActionStarted(action))
        try body
        finally listeners.foreach(_.writeIntentReadActionFinished(action))
      } finally {
        intent.unlock()
        listeners.foreach(_.afterWriteIntentReadActionFinished(action))
      }
    }
  }

  /** An editor notification can cancel reads before its queued write acquires the documents thread. */
  def cancelQueries(): Unit = queries.forEach(_.cancel())

  /** A read without its own cancellation (the platform's `runReadAction`): the lock only. */
  def readLocked[T](body: => T): T =
    if (isReading || hasWriteIntent) body
    else {
      val listeners = readListeners.asScala.toVector
      val action = HostLock.getClass
      listeners.foreach(_.beforeReadActionStart(action))
      lock.readLock.lock()
      try {
        listeners.foreach(_.readActionStarted(action))
        try body
        finally listeners.foreach(_.readActionFinished(action))
      } finally {
        lock.readLock.unlock()
        listeners.foreach(_.afterReadActionFinished(action))
      }
    }

  /** [[readLocked]] for Java callers. */
  def readLocked[T](body: java.util.concurrent.Callable[T]): T = readLocked(body.call())

  /** A query: a read that an edit cancels. Nested in another read, it is part of that one. */
  def query[T](body: => T): T =
    if (isReading || isWriting) body
    else {
      val indicator = new com.intellij.openapi.progress.EmptyProgressIndicator
      readLocked {
        queries.add(indicator)
        try com.intellij.openapi.progress.ProgressManager.getInstance.runProcess((() => body): com.intellij.openapi.util.Computable[T], indicator)
        finally queries.remove(indicator)
      }
    }

  def write[T](body: => T): T = {
    // A read lock cannot become a write lock: waiting for it here would wait for this thread's own read forever.
    if (isReading && !isWriting) throw new IllegalStateException("an edit cannot start inside a read")
    if (!isWriting) cancelQueries()
    writeIntent {
      if (isWriting) body
      else {
        val listeners = writeListeners.asScala.toVector
        val action = HostLock.getClass
        listeners.foreach(_.beforeWriteActionStart(action))
        lock.writeLock.lock()
        try {
          listeners.foreach(_.writeActionStarted(action))
          try body
          finally listeners.foreach(_.writeActionFinished(action))
        } finally {
          lock.writeLock.unlock()
          listeners.foreach(_.afterWriteActionFinished(action))
        }
      }
    }
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

  /** The result of `body` run on the dispatch thread, as the IDE runs the steps of a refactoring; its failure is rethrown here. */
  def call[T](body: => T): T =
    if (isCurrent) body
    else
      try executor.submit((() => body): java.util.concurrent.Callable[T]).get()
      catch { case e: java.util.concurrent.ExecutionException => throw e.getCause }

  /** Waits until the work scheduled so far has run. */
  def drain(): Unit = invokeAndWait(() => ())
}

/** A fixed-table adapter: a Proxy that answers only what `answer` handles; default methods run as written. */
// What an answer table gives for a call it has no case for. One lookup (applyOrElse) instead of two (isDefinedAt,
// then apply): tables answer the platform's most frequent calls.
private val NotAnswered = new AnyRef
private val Unanswered: Any => AnyRef = _ => NotAnswered

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

/** A request-local editor tracks positions through document and PSI changes, including imports before the caret. */
private[scalapsihost] final class RequestEditor(document: com.intellij.openapi.editor.Document,
                                                  project: com.intellij.openapi.project.Project, offset: Int) extends AutoCloseable {
  import com.intellij.openapi.editor.{Caret, CaretModel, Editor, EditorSettings, SelectionModel}
  import com.intellij.openapi.editor.ex.util.LexerEditorHighlighter
  import com.intellij.psi.PsiDocumentManager
  import org.jetbrains.plugins.scala.highlighter.ScalaSyntaxHighlighterFactory
  import com.intellij.openapi.editor.event.{DocumentEvent, DocumentListener}
  import com.intellij.openapi.editor.colors.EditorColorsManager
  import com.intellij.openapi.util.{Key, TextRange, UserDataHolderBase}

  private val data = new UserDataHolderBase
  private var caretOffset = offset
  private var selectionStart = offset
  private var selectionEnd = offset
  private lazy val settings = tableProxy(classOf[EditorSettings], "SelectionSettings") {
    // AbstractWordSelectioner and ScalaStringLiteralSelectioner read the platform's default camel-word mode.
    case ("isCamelWords", _) => java.lang.Boolean.FALSE
  }
  // Selection queries do not edit the document; the highlighter lexes its request-local snapshot.
  private lazy val highlighter = {
    val file = PsiDocumentManager.getInstance(project).getPsiFile(document)
    val syntax = ScalaSyntaxHighlighterFactory.createScalaSyntaxHighlighter(project, file.getVirtualFile, file.getLanguage)
    val result = new LexerEditorHighlighter(syntax, EditorColorsManager.getInstance.getGlobalScheme)
    result.setText(document.getImmutableCharSequence)
    result
  }
  private val listener = new DocumentListener {
    override def documentChanged(event: DocumentEvent): Unit = {
      def shifted(at: Int, right: Boolean): Int =
        if (at < event.getOffset || at == event.getOffset && !right) at
        else if (at > event.getOffset + event.getOldLength) at + event.getNewLength - event.getOldLength
        else event.getOffset + event.getNewLength
      val hadSelection = selectionStart != selectionEnd
      caretOffset = shifted(caretOffset, true)
      selectionStart = shifted(selectionStart, false)
      selectionEnd = shifted(selectionEnd, true)
      if (!hadSelection || selectionStart == selectionEnd) removeSelection()
    }
  }
  document.addDocumentListener(listener)

  private def move(at: Int): Unit = {
    require(at >= 0 && at <= document.getTextLength, s"caret outside document: $at")
    caretOffset = at
    removeSelection()
  }
  private def removeSelection(): Unit = { selectionStart = caretOffset; selectionEnd = caretOffset }
  private def select(start: Int, end: Int): Unit = {
    require(start >= 0 && start <= end && end <= document.getTextLength, "selection outside document")
    selectionStart = start
    selectionEnd = end
  }

  lazy val editor: Editor = tableProxy(classOf[Editor], "RequestEditor") {
    case ("getDocument", _) => document
    case ("getProject", _) => project
    case ("getCaretModel", _) => model
    case ("getSelectionModel", _) => selection
    // Word selection reads isCamelWords; it never changes editor settings.
    case ("getSettings", _) => settings
    // EditorActionUtil's lexeme boundaries and InjectedFileReferenceSelectioner's escapes use the layered lexer.
    case ("getHighlighter", _) => highlighter
    // A source editor, rather than a one-line input field.
    case ("isOneLineMode", _) => java.lang.Boolean.FALSE
    // The host has no editor-specific color scheme.
    case ("getColorsScheme", _) => EditorColorsManager.getInstance.getGlobalScheme
    case ("getUserData", Array(k: Key[?])) => data.getUserData(k).asInstanceOf[AnyRef]
    case ("putUserData", Array(k: Key[?], value)) => putData(k, value); null
  }
  // Reflection erases Key's type parameter, as in the project's user-data table.
  private def putData(key: Key[?], value: AnyRef): Unit = data.putUserData(key.asInstanceOf[Key[AnyRef]], value)
  private lazy val caret: Caret = tableProxy(classOf[Caret], "CompletionCaret") {
    case ("getEditor", _) => editor
    case ("getOffset", _) => Integer.valueOf(caretOffset)
    case ("getSelectionStart", _) => Integer.valueOf(selectionStart)
    case ("getSelectionEnd", _) => Integer.valueOf(selectionEnd)
    case ("hasSelection", _) => java.lang.Boolean.valueOf(selectionStart != selectionEnd)
    // SelectWordUtil checks whether the current selection still fits inside whitespace before expanding it.
    case ("getSelectionRange", _) => new TextRange(selectionStart, selectionEnd)
    case ("moveToOffset", Array(at: Integer)) => move(at.intValue); null
    case ("setSelection", Array(start: Integer, end: Integer)) => select(start.intValue, end.intValue); null
    case ("removeSelection", _) => removeSelection(); null
  }
  private lazy val model: CaretModel = tableProxy(classOf[CaretModel], "CompletionCaretModel") {
    case ("getCurrentCaret" | "getPrimaryCaret", _) => caret
    case ("getOffset", _) => Integer.valueOf(caretOffset)
    case ("getAllCarets", _) => java.util.List.of(caret)
    case ("moveToOffset", Array(at: Integer)) => move(at.intValue); null
  }
  private lazy val selection: SelectionModel = tableProxy(classOf[SelectionModel], "CompletionSelection") {
    case ("getSelectionStart", _) => Integer.valueOf(selectionStart)
    case ("getSelectionEnd", _) => Integer.valueOf(selectionEnd)
    case ("hasSelection", _) => java.lang.Boolean.valueOf(selectionStart != selectionEnd)
    case ("setSelection", Array(start: Integer, end: Integer)) => select(start.intValue, end.intValue); null
    case ("removeSelection", _) => removeSelection(); null
  }

  def close(): Unit = document.removeDocumentListener(listener)
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
    if (nulls.contains(cls)) { CallLog.answered(s"$label.service(null) ${cls.getName}"); null }
    else {
      val make = factories.get(cls)
      if (make == null) CallLog.unsupported(s"$label.getService ${cls.getName}")
      else {
        CallLog.answered(s"$label.service ${cls.getName}")
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

/** The platform's compact set looks up the VFS manager even for files without ids; the host keeps files directly. */
private[scalapsihost] final class HostVirtualFileSet(private var files: java.util.Set[com.intellij.openapi.vfs.VirtualFile])
    extends java.util.AbstractSet[com.intellij.openapi.vfs.VirtualFile] with com.intellij.openapi.vfs.VirtualFileSet {
  def this() = this(new java.util.LinkedHashSet)

  override def iterator(): java.util.Iterator[com.intellij.openapi.vfs.VirtualFile] = files.iterator()
  override def size(): Int = files.size
  override def contains(o: Object): Boolean = files.contains(o)
  override def add(file: com.intellij.openapi.vfs.VirtualFile): Boolean = files.add(file)
  override def remove(o: Object): Boolean = files.remove(o)
  override def freezed(): java.util.Set[com.intellij.openapi.vfs.VirtualFile] = {
    freeze()
    this
  }
  override def freeze(): Unit = files = java.util.Collections.unmodifiableSet(files)
  override def process(processor: com.intellij.util.Processor[? >: com.intellij.openapi.vfs.VirtualFile]): Boolean =
    files.asScala.forall(processor.process)
}
