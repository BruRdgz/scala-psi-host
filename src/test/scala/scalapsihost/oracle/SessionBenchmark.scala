package scalapsihost.oracle

import scalapsihost.lsp.ProjectSession
import scalapsihost.{HostConfig, LineColumn, ScalaPsiHost}

import java.io.File
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Latency and memory of a project session, as the language server runs it: opens the project over BSP, answers
 * requests at identifiers sampled across its Scala files, then runs a long session of edits and heap samples. The
 * project's files are only read; edits stay in memory.
 *
 * Usage: `Test/runMain scalapsihost.oracle.SessionBenchmark <project dir> <report.md> [--positions=N] [--rounds=N]
 * [--edit-file=<path in the project>] [--connection=<name>]`. Positions (default 1500) are identifiers spread evenly
 * over the files; each round (default 60) edits a file (a comment appended, then removed), asks for its problems and
 * semantic tokens, and samples the heap after a full GC. Rounds edit the files in turn, or always `--edit-file` (heap
 * that still grows then is held per edit, not by caches of more files).
 */
object SessionBenchmark {
  private val Identifier = "[A-Za-z_][A-Za-z0-9_]*".r

  def main(args: Array[String]): Unit = {
    val (Seq(projectArg, reportPath), flags) = args.toSeq.partition(!_.startsWith("--"))
    def flag(name: String) = flags.collectFirst { case f if f.startsWith(s"--$name=") => f.stripPrefix(s"--$name=") }
    val positionCount = flag("positions").map(_.toInt).getOrElse(1500)
    val rounds = flag("rounds").map(_.toInt).getOrElse(60)
    val projectDir = Path.of(projectArg).toAbsolutePath
    val editFile = flag("edit-file").map(projectDir.resolve)
    val host = ScalaPsiHost.start(HostConfig(
      stateDir = Path.of(sys.props("scalapsihost.stateDir")),
      scalaPluginJars = sys.props("scalapsihost.scalaPluginJars").split(File.pathSeparator).toSeq.map(Path.of(_)),
    ))

    val (session, openMs) = timed(ProjectSession.open(host, projectDir, flag("connection")))
    val heapAfterOpenMb = usedHeapMb()
    try {
      val scalaFiles = session.sourceFiles.filter(_.toString.endsWith(".scala")).sorted
      val identifiers = scalaFiles.flatMap { path =>
        val lines = session.text(path).split("\n", -1).toSeq
        for ((line, l) <- lines.zipWithIndex if !line.trim.startsWith("//") && !line.trim.startsWith("*"); m <- Identifier.findAllMatchIn(line))
          yield (path, LineColumn(l, m.start))
      }
      val step = math.max(1, identifiers.size / positionCount)
      val positions = identifiers.indices.by(step).map(identifiers).take(positionCount)

      val latencies = scala.collection.mutable.LinkedHashMap.empty[String, Vector[Long]]
      def measure[T](kind: String)(request: => T): Unit = {
        val (_, ms) = timed(request)
        latencies(kind) = latencies.getOrElse(kind, Vector.empty) :+ ms
      }
      // Each file's problems first, as an editor opening it: later requests find the plugin's caches as they are then.
      for (path <- scalaFiles) measure("problems (per file)")(session.problems(path))
      for (path <- scalaFiles) {
        measure("semantic tokens (per file)")(session.semanticTokens(path))
        measure("document symbols (per file)")(session.outline(path))
        measure("folding ranges (per file)")(session.foldingRanges(path))
      }
      for ((path, at) <- positions) {
        measure("hover")(session.hover(path, at))
        measure("definition")(session.definition(path, at))
        measure("document highlights")(session.highlights(path, at))
      }
      val heapAfterRequestsMb = usedHeapMb()

      val heapByRound = (0 until rounds).map { round =>
        val path = editFile.getOrElse(scalaFiles(round % scalaFiles.size))
        val lines = session.text(path).split("\n", -1)
        val end = LineColumn(lines.length - 1, lines.last.length)
        session.change(path, Some(end -> end), "\n// edit")
        measure("problems after an edit")(session.problems(path))
        measure("semantic tokens after an edit")(session.semanticTokens(path))
        session.change(path, Some(end -> LineColumn(end.line + 1, "// edit".length)), "")
        usedHeapMb()
      }

      val lines = Vector.newBuilder[String]
      lines += s"# Session benchmark: ${projectDir.getFileName}"
      lines += ""
      lines += s"JDK ${System.getProperty("java.version")}, max heap ${Runtime.getRuntime.maxMemory / (1024 * 1024)} MB, one thread."
      lines += ""
      lines += s"- Scala files: ${scalaFiles.size}; sampled positions: ${positions.size}; edit rounds: $rounds" +
        editFile.fold(" (files in turn)")(f => s" (always ${projectDir.relativize(f)})")
      lines += s"- opening the project (BSP import, library sources, modules, sources): $openMs ms; heap after: $heapAfterOpenMb MB"
      lines += s"- heap after the requests: $heapAfterRequestsMb MB"
      lines += s"- heap over the edit rounds (after a full GC): first ${heapByRound.headOption.getOrElse(0L)} MB, " +
        s"max ${if (heapByRound.isEmpty) 0 else heapByRound.max} MB, last ${heapByRound.lastOption.getOrElse(0L)} MB"
      lines += ""
      lines += "| Request | Count | p50 ms | p95 ms | p99 ms | max ms |"
      lines += "|---|---|---|---|---|---|"
      for ((kind, ms) <- latencies) {
        val sorted = ms.sorted
        def percentile(p: Double) = sorted(math.min(sorted.size - 1, (p * sorted.size).toInt))
        lines += s"| $kind | ${sorted.size} | ${percentile(0.5)} | ${percentile(0.95)} | ${percentile(0.99)} | ${sorted.last} |"
      }
      lines += ""
      lines += "## Heap by edit round (MB)"
      lines += ""
      lines += heapByRound.mkString(", ")
      Files.createDirectories(Path.of(reportPath).toAbsolutePath.getParent)
      Files.write(Path.of(reportPath), lines.result().asJava)
      println(s"session benchmark: report at $reportPath")
    } finally session.close()
    // The platform keeps non-daemon threads running.
    sys.exit(0)
  }

  private def timed[T](body: => T): (T, Long) = {
    val start = System.nanoTime()
    val result = body
    (result, (System.nanoTime() - start) / 1_000_000)
  }

  private def usedHeapMb(): Long = {
    System.gc()
    val rt = Runtime.getRuntime
    (rt.totalMemory() - rt.freeMemory()) / (1024 * 1024)
  }
}
