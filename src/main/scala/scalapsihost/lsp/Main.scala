package scalapsihost.lsp

import org.eclipse.lsp4j.launch.LSPLauncher
import scalapsihost.{HostConfig, HostLog, ScalaPsiHost}

import java.io.File
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters.*

/**
 * Runs the language server over stdin and stdout.
 *
 * System properties: `scalapsihost.stateDir` (where the platform keeps its state) and `scalapsihost.scalaPluginJars`
 * (the Scala plugin's jars, which must also be on the classpath). Environment: `SCALA_PSI_HOST_LOG=debug` logs every
 * request.
 *
 * The log is written to `<stateDir>/logs/scala-psi-host-<start time>.log` (the latest logs are kept) and sent to the
 * client.
 */
object Main {
  // Logs of earlier runs kept next to the current one.
  private val KeptLogs = 10

  def main(args: Array[String]): Unit = {
    // Stdout carries the protocol: anything else printed there would corrupt it, so it goes to stderr.
    val protocolOut = System.out
    System.setOut(System.err)
    def property(name: String) = sys.props.getOrElse(name, throw new IllegalArgumentException(s"system property $name is not set"))
    val stateDir = Path.of(property("scalapsihost.stateDir"))
    if (sys.env.get("SCALA_PSI_HOST_LOG").exists(_.equalsIgnoreCase("debug"))) HostLog.threshold = HostLog.Level.Debug
    val logFile = openLogFile(stateDir.resolve("logs"))
    // What is logged before the client connects (the host's startup) is sent to it when it connects.
    val startup = new java.util.concurrent.ConcurrentLinkedQueue[HostLog.Entry]()
    val buffering = HostLog.addSink(entry => startup.add(entry))

    val heap = Runtime.getRuntime.maxMemory / (1024 * 1024)
    HostLog.info(s"scala-psi-host 0.1 starting: JDK ${System.getProperty("java.version")}, heap max $heap MB, state $stateDir, log $logFile")
    val host = HostLog.timed("host started")(ScalaPsiHost.start(HostConfig(
      stateDir = stateDir,
      scalaPluginJars = property("scalapsihost.scalaPluginJars").split(File.pathSeparator).toSeq.map(Path.of(_)),
    )))
    buffering.close()
    // The platform keeps threads of its own running: the process ends when the client says so.
    val server = new ScalaLanguageServer(host, code => sys.exit(code), Some(logFile), startup.asScala.toSeq)
    val launcher = new LSPLauncher.Builder[HostProtocol.Client]()
      .setLocalService(server)
      .setRemoteInterface(classOf[HostProtocol.Client])
      .setInput(System.in)
      .setOutput(protocolOut)
      .create()
    server.connect(launcher.getRemoteProxy)
    launcher.startListening().get()
  }

  /** A new log file in `directory`, written as entries are logged; the oldest logs beyond [[KeptLogs]] are deleted. */
  private def openLogFile(directory: Path): Path = {
    Files.createDirectories(directory)
    val name = s"scala-psi-host-${LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"))}-${ProcessHandle.current.pid}.log"
    val file = directory.resolve(name)
    val earlier = scala.util.Using.resource(Files.list(directory))(_.iterator.asScala.toVector)
      .filter(_.getFileName.toString.startsWith("scala-psi-host-")).sorted
    earlier.dropRight(KeptLogs - 1).foreach(Files.deleteIfExists)
    val out = Files.newBufferedWriter(file, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
    HostLog.addSink { entry =>
      out.synchronized {
        out.write(entry.toString)
        out.newLine()
        out.flush()
      }
    }
    file
  }
}
