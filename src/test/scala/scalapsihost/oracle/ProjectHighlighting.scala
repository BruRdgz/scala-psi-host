package scalapsihost.oracle

import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.util.TextRange
import scalapsihost.{BspImport, Diagnostic, HostConfig, LineColumn, ScalaPsiHost}

import java.io.File
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * Highlights a whole project through the host as an LSP server would: imports the build over BSP, compiles it there,
 * adds its modules, opens every Scala source and reports each file's errors (the plugin's, and the compiler's for files
 * whose errors come from the compiler), with timings and heap use. For a project that compiles, every reported error
 * is a false positive (of the host, or of the plugin itself).
 *
 * Usage: `Test/runMain scalapsihost.oracle.ProjectHighlighting <project dir> <report.md> [--connection=<name>]
 * [--plugin-errors]`. `--plugin-errors` takes every file's errors from the plugin's type-aware checks instead.
 */
object ProjectHighlighting {
  def main(args: Array[String]): Unit = {
    val (Seq(projectArg, reportPath), flags) = args.toSeq.partition(!_.startsWith("--"))
    val projectDir = Path.of(projectArg).toAbsolutePath
    val connection = flags.collectFirst { case f if f.startsWith("--connection=") => f.stripPrefix("--connection=") }
    val compilerErrors = !flags.contains("--plugin-errors")
    val host = ScalaPsiHost.start(HostConfig(
      stateDir = Path.of(sys.props("scalapsihost.stateDir")),
      scalaPluginJars = sys.props("scalapsihost.scalaPluginJars").split(File.pathSeparator).toSeq.map(Path.of(_)),
      compilerErrorsScala3 = compilerErrors,
    ))

    val t0 = System.nanoTime()
    val build = BspImport.connect(projectDir, connection)
    val (imported, compiled) =
      try {
        val imported = build.modules()
        (imported, if (compilerErrors) Some(build.compile()) else None)
      } finally build.close()
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
        try {
          val fromPlugin = host.diagnostics(file).map(d => ("plugin", d))
          val fromCompiler =
            if (!host.isCompilerHighlighted(file)) Seq.empty
            else compiled.toSeq.flatMap(_.diagnostics.getOrElse(projectDir.resolve(path), Seq.empty)).map { c =>
              ("compiler", Diagnostic(c.severity, new TextRange(offset(file.getText, c.start), offset(file.getText, c.end)), c.message))
            }
          Right((fromPlugin ++ fromCompiler).filter(_._2.severity == HighlightSeverity.ERROR))
        }
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
    lines += s"- errors from: ${if (compilerErrors) "the compiler for Scala 3 files (build compile status: " + compiled.map(c => if (c.succeeded) "OK" else "failed").getOrElse("-") + ")" else "the plugin's type-aware checks"}"
    lines += s"- BSP import${if (compilerErrors) " + compile" else ""}: $importMs ms; adding modules (mount + index jars): $modulesMs ms, heap after: $heapAfterModulesMb MB"
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
    val messages = results.flatMap(_.outcome.toOption.toSeq.flatten).map((origin, d) => s"[$origin] " + d.message.replaceAll("'[^']*'", "'…'").take(90))
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
        case Right(ds) => ds.map((origin, d) => s"${d.range} [$origin] ${d.message}").mkString("<br>")
      }
      lines += s"| ${r.path} | ${cell(text)} |"
    }
    Files.createDirectories(Path.of(reportPath).toAbsolutePath.getParent)
    Files.write(Path.of(reportPath), lines.result().asJava)
    println(s"project highlighting: ${results.count(r => r.outcome.exists(_.isEmpty))} / ${results.size} files without errors; report at $reportPath")
  }

  /** Each error with where it came from: `plugin` or `compiler`. */
  private final case class FileResult(path: String, outcome: Either[String, Seq[(String, Diagnostic)]], ms: Long)

  private def offset(text: String, position: LineColumn): Int = {
    val lineStart = Iterator.iterate(0)(i => text.indexOf('\n', i) + 1).drop(position.line).next()
    math.min(lineStart + position.column, text.length)
  }

  private def millisSince(start: Long): Long = (System.nanoTime() - start) / 1_000_000

  private def usedHeapMb(): Long = {
    System.gc()
    val rt = Runtime.getRuntime
    (rt.totalMemory() - rt.freeMemory()) / (1024 * 1024)
  }
}
