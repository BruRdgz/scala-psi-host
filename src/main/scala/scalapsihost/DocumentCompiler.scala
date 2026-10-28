package scalapsihost

import com.intellij.lang.annotation.HighlightSeverity

import java.io.File
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

/**
 * Compiles one document of a Scala 3 module with the module's own compiler, as the IDE's document compilation does
 * for compiler-based highlighting: the document's current text, against the module's classpath and class output,
 * up to the last phase that reports errors (before code generation), as the plugin's DocumentCompiler stops it.
 *
 * The compiler runs in a class loader of its own per set of compiler jars (kept, so later compilations reuse the
 * loaded compiler), apart from the host's own Scala, through the compiler's Java interface
 * (`dotty.tools.dotc.interfaces`). Scala 2 has no such interface; its files' errors come from the build's compile.
 */
final class DocumentCompiler {
  private val loaders = new ConcurrentHashMap[Seq[Path], ClassLoader]()

  /** The compiler's problems in `text`, the content of the module's file named `fileName`. */
  def compile(spec: CompileSpec, fileName: String, text: String): Seq[CompilerDiagnostic] = {
    if (!spec.scalaVersion.startsWith("3."))
      throw new UnsupportedOperationException(
        s"document compilation needs a Scala 3 module (its compiler's Java interface); this one is on Scala ${spec.scalaVersion}")
    val loader = loaders.computeIfAbsent(spec.compilerJars,
      jars => new URLClassLoader(jars.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader))
    val workDir = Files.createTempDirectory("scala-psi-host-document")
    try {
      val source = workDir.resolve(fileName)
      Files.writeString(source, text)
      val output = Files.createDirectory(workDir.resolve("classes"))
      val args =
        Seq("-classpath", (spec.classpath :+ spec.classDirectory).mkString(File.pathSeparator), "-d", output.toString) ++
          highlightingOptions(spec) :+ source.toString
      runCompiler(loader, args)
    } finally {
      Files.walk(workDir).sorted(Comparator.reverseOrder()).forEach(Files.delete)
    }
  }

  // As the plugin's DocumentCompiler for Scala 3: stop before code generation, and report unused imports from 3.3 on.
  private def highlightingOptions(spec: CompileSpec): Seq[String] = {
    val stopAfter =
      if (spec.scalacOptions.exists(_.startsWith("-Ystop-after:"))) Seq.empty else Seq("-Ystop-after:repeatableAnnotations")
    val unusedImports =
      if (atLeast3_3(spec.scalaVersion) && !spec.scalacOptions.exists(o => o.startsWith("-Wunused:") && (o.contains("imports") || o.contains("all"))))
        Seq("-Wunused:imports")
      else Seq.empty
    spec.scalacOptions ++ stopAfter ++ unusedImports
  }

  private def atLeast3_3(version: String): Boolean = version.split('.') match {
    case Array("3", minor, _*) => minor.toIntOption.exists(_ >= 3)
    case _ => false
  }

  private def runCompiler(loader: ClassLoader, args: Seq[String]): Seq[CompilerDiagnostic] = {
    val reporterInterface = loader.loadClass("dotty.tools.dotc.interfaces.SimpleReporter")
    val callbackInterface = loader.loadClass("dotty.tools.dotc.interfaces.CompilerCallback")
    val diagnosticInterface = loader.loadClass("dotty.tools.dotc.interfaces.Diagnostic")
    val positionInterface = loader.loadClass("dotty.tools.dotc.interfaces.SourcePosition")
    def call(target: AnyRef, interface: Class[?], method: String): AnyRef = interface.getMethod(method).invoke(target)

    val reported = mutable.ArrayBuffer.empty[CompilerDiagnostic]
    val reporter: InvocationHandler = (proxy: AnyRef, method: Method, args: Array[AnyRef]) => method.getName match {
      case "report" =>
        val diagnostic = args(0)
        val position = call(diagnostic, diagnosticInterface, "position").asInstanceOf[java.util.Optional[AnyRef]]
        val level = severity(call(diagnostic, diagnosticInterface, "level").asInstanceOf[Int])
        val message = call(diagnostic, diagnosticInterface, "message").asInstanceOf[String]
        // A problem without a position is about the run, not the document. An error (e.g. a bad option) explains why
        // the document has no other problems, so it is reported at the start of the file; anything else (the
        // compiler's "N errors found" summary is a warning) is only logged.
        def at(line: String, column: String) =
          if (position.isEmpty) LineColumn(0, 0)
          else LineColumn(call(position.get, positionInterface, line).asInstanceOf[Int], call(position.get, positionInterface, column).asInstanceOf[Int])
        if (position.isPresent || level == HighlightSeverity.ERROR)
          reported += CompilerDiagnostic(level, at("startLine", "startColumn"), at("endLine", "endColumn"), message)
        else System.err.println(s"scala-psi-host: compiler: $message")
        null
      case "hashCode" => Integer.valueOf(System.identityHashCode(proxy))
      case "equals" => java.lang.Boolean.valueOf(proxy eq args(0))
      case "toString" => "HostDocumentReporter"
    }

    // The compiler's entry point is the object dotty.tools.dotc.Main, whose instance lives in its module class.
    val mainModule = loader.loadClass("dotty.tools.dotc.Main$")
    val process = mainModule.getMethod("process", classOf[Array[String]], reporterInterface, callbackInterface)
    val thread = Thread.currentThread
    val previousLoader = thread.getContextClassLoader
    thread.setContextClassLoader(loader)
    try process.invoke(mainModule.getField("MODULE$").get(null), args.toArray, Proxy.newProxyInstance(loader, Array(reporterInterface), reporter), null)
    finally thread.setContextClassLoader(previousLoader)
    reported.toSeq
  }

  // dotty.tools.dotc.interfaces.Diagnostic: ERROR = 2, WARNING = 1, INFO = 0.
  private def severity(level: Int): HighlightSeverity = level match {
    case 2 => HighlightSeverity.ERROR
    case 1 => HighlightSeverity.WARNING
    case _ => HighlightSeverity.INFORMATION
  }
}
