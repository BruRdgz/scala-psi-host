package scalapsihost.oracle

import com.intellij.lang.annotation.HighlightSeverity
import scalapsihost.{BspImport, Diagnostic, HostConfig, ScalaPsiHost}

import java.io.File
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * Highlights a whole project through the host as an LSP server would: imports the build over BSP, adds its modules,
 * opens every Scala source and reports each file's errors, with timings and heap use. For a project that compiles,
 * every reported error is a false positive (of the host, or of the plugin itself).
 *
 * Usage: `Test/runMain scalapsihost.oracle.ProjectHighlighting <project dir> <report.md> [--connection=<name>]`.
 */
object ProjectHighlighting {
  def main(args: Array[String]): Unit = {
    val (Seq(projectArg, reportPath), flags) = args.toSeq.partition(!_.startsWith("--"))
    val projectDir = Path.of(projectArg).toAbsolutePath
    val connection = flags.collectFirst { case f if f.startsWith("--connection=") => f.stripPrefix("--connection=") }
    val host = ScalaPsiHost.start(HostConfig(
      stateDir = Path.of(sys.props("scalapsihost.stateDir")),
      scalaPluginJars = sys.props("scalapsihost.scalaPluginJars").split(File.pathSeparator).toSeq.map(Path.of(_)),
    ))

    val t0 = System.nanoTime()
    val imported = BspImport.importBuild(projectDir, connection)
    val importMs = millisSince(t0)

    val t1 = System.nanoTime()
    host.addJdk(Path.of(System.getProperty("java.home")))
    // Each jar indexed here, once, to report it; adding the modules then reuses the mounted jars.
    val jarReports = imported.flatMap(_.spec.classpath).distinct.map(jar => jar -> host.addLibrary(jar))
    val modules = imported.map(m => m -> host.addModule(m.spec))
    val modulesMs = millisSince(t1)
    val heapAfterModulesMb = usedHeapMb()

    val t2 = System.nanoTime()
    val files = for ((m, hostModule) <- modules; source <- m.sources)
      yield projectDir.relativize(source).toString -> host.openSource(Files.readString(source), hostModule)
    val openMs = millisSince(t2)

    val t3 = System.nanoTime()
    val results = files.map { (path, file) =>
      val start = System.nanoTime()
      val outcome =
        try Right(host.diagnostics(file).filter(_.severity == HighlightSeverity.ERROR))
        catch { case NonFatal(e) => println(s"crash in $path: $e"); Left(e.toString) }
      FileResult(path, outcome, millisSince(start))
    }
    val highlightMs = millisSince(t3)
    val heapAfterMb = usedHeapMb()

    val lines = Vector.newBuilder[String]
    def cell(s: String) = s.replace("|", "\\|").replace("\n", " ")
    lines += s"# Project highlighting: ${projectDir.getFileName}"
    lines += ""
    lines += s"JDK ${System.getProperty("java.version")}, production mode."
    lines += ""
    lines += s"- modules: ${imported.size}; jars: ${imported.flatMap(_.spec.classpath).distinct.size}; Scala files: ${files.size}"
    lines += s"- files without errors: ${results.count(r => r.outcome.exists(_.isEmpty))} / ${results.size}; crashed: ${results.count(_.outcome.isLeft)}"
    lines += s"- errors: ${results.map(_.outcome.fold(_ => 0, _.size)).sum}"
    lines += s"- BSP import: $importMs ms; adding modules (mount + index jars): $modulesMs ms, heap after: $heapAfterModulesMb MB"
    lines += s"- opening sources (parse + index): $openMs ms"
    lines += s"- highlighting (one thread): $highlightMs ms, heap after: $heapAfterMb MB"
    lines += s"- slowest files: ${results.sortBy(-_.ms).take(5).map(r => s"${r.path} ${r.ms} ms").mkString(", ")}"
    lines += ""
    lines += "## Jars"
    lines += ""
    lines += "| Jar | Class files | Scala indexed | Java | Refused |"
    lines += "|---|---|---|---|---|"
    for ((jar, r) <- jarReports)
      lines += s"| ${jar.getFileName} | ${r.classFiles} | ${r.scalaIndexed} | ${r.javaClasses} | ${r.refused.size} |"
    lines += ""
    lines += "## Errors by message"
    lines += ""
    val messages = results.flatMap(_.outcome.toOption.toSeq.flatten).map(d => d.message.replaceAll("'[^']*'", "'…'").take(90))
    for ((message, count) <- messages.groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2).take(30))
      lines += s"- $count × ${cell(message)}"
    lines += ""
    lines += "## Files"
    lines += ""
    lines += "| File | Errors |"
    lines += "|---|---|"
    for (r <- results if !r.outcome.exists(_.isEmpty)) {
      val text = r.outcome match {
        case Left(crash) => s"crash: $crash"
        case Right(ds) => ds.map(d => s"${d.range} ${d.message}").mkString("<br>")
      }
      lines += s"| ${r.path} | ${cell(text)} |"
    }
    Files.createDirectories(Path.of(reportPath).toAbsolutePath.getParent)
    Files.write(Path.of(reportPath), lines.result().asJava)
    println(s"project highlighting: ${results.count(r => r.outcome.exists(_.isEmpty))} / ${results.size} files without errors; report at $reportPath")
  }

  private final case class FileResult(path: String, outcome: Either[String, Seq[Diagnostic]], ms: Long)

  private def millisSince(start: Long): Long = (System.nanoTime() - start) / 1_000_000

  private def usedHeapMb(): Long = {
    System.gc()
    val rt = Runtime.getRuntime
    (rt.totalMemory() - rt.freeMemory()) / (1024 * 1024)
  }
}
