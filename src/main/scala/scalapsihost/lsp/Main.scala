package scalapsihost.lsp

import org.eclipse.lsp4j.launch.LSPLauncher
import scalapsihost.{HostConfig, ScalaPsiHost}

import java.io.File
import java.nio.file.Path

/**
 * Runs the language server over stdin and stdout.
 *
 * System properties: `scalapsihost.stateDir` (where the platform keeps its state) and `scalapsihost.scalaPluginJars`
 * (the Scala plugin's jars, which must also be on the classpath).
 */
object Main {
  def main(args: Array[String]): Unit = {
    // Stdout carries the protocol: anything else printed there would corrupt it, so it goes to stderr.
    val protocolOut = System.out
    System.setOut(System.err)
    def property(name: String) = sys.props.getOrElse(name, throw new IllegalArgumentException(s"system property $name is not set"))
    val host = ScalaPsiHost.start(HostConfig(
      stateDir = Path.of(property("scalapsihost.stateDir")),
      scalaPluginJars = property("scalapsihost.scalaPluginJars").split(File.pathSeparator).toSeq.map(Path.of(_)),
    ))
    val server = new ScalaLanguageServer(host)
    val launcher = LSPLauncher.createServerLauncher(server, System.in, protocolOut)
    server.connect(launcher.getRemoteProxy)
    launcher.startListening().get()
  }
}
