package scalapsihost

import scala.jdk.CollectionConverters.*

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

  /**
   * Serializes potential writers while allowing concurrent readers; reentrant on the owning thread.
   *
   * @throws IllegalStateException when entered from a read without an existing write intent
   */
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

  /**
   * Runs a cancellable read. Nested reads share the outer query's indicator and lock.
   *
   * @throws com.intellij.openapi.progress.ProcessCanceledException when a pending edit cancels the query
   */
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

  /**
   * Cancels running queries and executes `body` under write intent and the exclusive write lock.
   * Nested writes reuse the owning thread's locks.
   *
   * @throws IllegalStateException when a read attempts to acquire the write lock
   */
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
