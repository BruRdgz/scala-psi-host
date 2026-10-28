package scalapsihost

import java.io.{PrintWriter, StringWriter}
import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}
import java.util.concurrent.atomic.AtomicLong

/**
 * The host's log: what the host and the language server do (startup, import, requests) and what the platform and the
 * plugin log (see [[HostLogger]]). Entries go to the sinks added (the language server's log file and its client);
 * with none, to stderr. Each level's entries are counted, and the latest warnings and errors kept, for status reports.
 */
object HostLog {
  enum Level { case Debug, Info, Warn, Error }

  final case class Entry(time: Instant, level: Level, message: String) {
    override def toString: String = s"${HostLog.TimeFormat.format(time)} ${level.toString.toUpperCase.padTo(5, ' ')} $message"
  }

  private val TimeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS").withZone(ZoneId.systemDefault())
  // Enough recent warnings and errors to see what went wrong without reading the log file.
  private val RecentProblems = 50

  /** Entries below this level are dropped (`SCALA_PSI_HOST_LOG=debug` in the language server). */
  @volatile var threshold: Level = Level.Info

  private val sinks = new CopyOnWriteArrayList[Entry => Unit]()
  private val counts = new ConcurrentHashMap[Level, AtomicLong]()
  private val recent = new java.util.ArrayDeque[Entry]()

  /** Adds a sink; closing the result removes it. */
  def addSink(sink: Entry => Unit): AutoCloseable = {
    sinks.add(sink)
    () => sinks.remove(sink)
  }

  def isDebug: Boolean = threshold == Level.Debug

  def debug(message: => String): Unit = if (isDebug) log(Level.Debug, message, null)
  def info(message: String): Unit = log(Level.Info, message, null)
  def warn(message: String, cause: Throwable = null): Unit = log(Level.Warn, message, cause)
  def error(message: String, cause: Throwable = null): Unit = log(Level.Error, message, cause)

  /** Runs `body` and logs how long `what` took. */
  def timed[T](what: String)(body: => T): T = {
    val start = System.nanoTime()
    val result = body
    info(s"$what in ${(System.nanoTime() - start) / 1_000_000} ms")
    result
  }

  def count(level: Level): Long = Option(counts.get(level)).fold(0L)(_.get)

  /** The latest warnings and errors, oldest first. */
  def recentProblems: Seq[Entry] = recent.synchronized(recent.toArray(Array.empty[Entry]).toSeq)

  private def log(level: Level, message: String, cause: Throwable): Unit =
    if (level.ordinal >= threshold.ordinal) {
      val text = if (cause == null) message else {
        val trace = new StringWriter
        cause.printStackTrace(new PrintWriter(trace))
        s"$message\n$trace".stripTrailing
      }
      val entry = Entry(Instant.now(), level, text)
      counts.computeIfAbsent(level, _ => new AtomicLong()).incrementAndGet()
      if (level.ordinal >= Level.Warn.ordinal) recent.synchronized {
        recent.addLast(entry)
        if (recent.size > RecentProblems) recent.removeFirst()
      }
      if (sinks.isEmpty) System.err.println(entry)
      else sinks.forEach { sink =>
        // A sink that fails (a client gone) cannot log its own failure: it goes to stderr, the entry with it.
        try sink(entry)
        catch { case scala.util.control.NonFatal(e) => System.err.println(s"$entry\n(log sink failed: $e)") }
      }
    }
}
