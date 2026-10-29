package scalapsihost.oracle

import com.intellij.psi.util.PsiTreeUtil
import org.eclipse.lsp4j.{CompletionItemKind, CompletionItemTag}
import org.jetbrains.plugins.scala.LatestScalaVersions
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import scalapsihost.{CallLog, Completion, HostConfig, HostLog, HostModule, ModuleSpec, ScalaPsiHost}

import java.io.File
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/**
 * Replays file-based completion expectations without their IDE lookup UI. Generated test classes select cases;
 * BASIC and keyword fixtures use Scala 2.13, the first version their supportedIn admits. SMART is reported as skipped.
 *
 * Usage: `Test/runMain scalapsihost.oracle.CompletionOracle <intellij-scala checkout> <report.md> [--only=<name>,...]`.
 */
object CompletionOracle {
  private final case class TestCase(suite: String, name: String, path: Path, skip: Option[String])
  private final case class Result(outcome: String, reason: String)

  def main(args: Array[String]): Unit = {
    val (Seq(repo, reportPath), flags) = args.toSeq.partition(!_.startsWith("--"))
    val pluginRoot = Path.of(repo, "scala/scala-impl")
    val host = ScalaPsiHost.start(HostConfig(
      stateDir = Path.of(sys.props("scalapsihost.stateDir")),
      scalaPluginJars = sys.props("scalapsihost.scalaPluginJars").split(File.pathSeparator).toSeq.map(Path.of(_)),
      unitTestMode = true,
    ))
    host.addJdk(Path.of(System.getProperty("java.home")))
    host.addLibrary(Path.of(sys.props("scalapsihost.fixture.scalaLibrary")))
    val module = host.addModule(ModuleSpec("completion-oracle", "2.13.18", Seq.empty,
      Seq(Path.of(sys.props("scalapsihost.fixture.scalaLibrary")))))
    val only = flags.collectFirst { case f if f.startsWith("--only=") => f.stripPrefix("--only=").split(',').toSet }
    val cases = host.read(collect(host, pluginRoot)).filter(c => only.forall(_.contains(c.name)))
    val started = System.nanoTime()
    val results = cases.map(c => c -> c.skip.fold(runCase(host, module, c))(r => Result("skip", r)))
    val lines = Vector.newBuilder[String]
    lines += "# Completion oracle"
    lines += ""
    lines += s"Scala 2.13.18, unit-test mode, JDK ${System.getProperty("java.version")}; ${(System.nanoTime() - started) / 1_000_000} ms."
    lines += ""
    lines += "Expected lookup membership (including strikeout and multiplicity) is compared with the upstream file fixture. " +
      "Keyword cases compare only KeywordInsertHandler items, as their base does. No file-based suite asserts relevance order; " +
      "completion3 uses test-method expectations and is out of scope, including its order tests. The LSP cap is disabled for replay."
    lines += ""
    lines += "The host is always smart; keyword fixtures declare DUMB_EMPTY_INDEX. Only their keyword results are compared. " +
      "Java binary short-name enumeration, auto-import, SMART and insert-handler edits are outside this replay's supported surface."
    lines += ""
    lines += "| Suite | Agree | Disagree | Skip |"
    lines += "|---|---:|---:|---:|"
    for (suite <- Seq("completion", "keywordCompletion")) {
      val outcomes = results.filter(_._1.suite == suite).map(_._2.outcome)
      lines += s"| $suite | ${outcomes.count(_ == "agree")} | ${outcomes.count(_ == "disagree")} | ${outcomes.count(_ == "skip")} |"
    }
    lines += ""
    lines += "## Runtime deviations from the design study"
    lines += ""
    lines += "- OffsetsInFile.copyWithReplacement reached a PSI write transaction and FileManager.forceReload. " +
      "The driver instead parses a request-local PsiFileFactory snapshot with original-file/module context and Scala features, " +
      "retaining the platform OffsetMap and OffsetTranslator lifetimes."
    lines += "- BaseCompletionService needs a thread-local current process for concurrent host reads. " +
      "The six platform completion weigher anchors are registered alongside all seven declared Scala completion/proximity weighers; statistics and ML are not applied."
    lines += "- Runtime required ScalaNamesValidator (the Java fallback broke operator/keyword dummy text), " +
      "bean instantiation without injection, empty modcompletion/test-framework/main-method/package-directory EPs, " +
      "and name/package/migration/annotation presentation services. All four planned contributors are retained; none were added."
    lines += "- File-based replay needs physical source files. Synthetic ScalaPsiElementFactory files have empty VFS contents; " +
      "the driver supplies a request-local initialization snapshot for those callers too. Keyword indexing mode remains smart in the host."
    lines += "- The mixed-project benchmark additionally required Java source-membership/language-level lookup for smart-pointer icons " +
      "and nullable PSI text in deterministic tie-breaking. Synthetic method ties use their type signatures. " +
      "Constructor builders can import without shouldImport; builders with nontrivial handlers are conservatively excluded and counted."
    lines += ""
    lines += "Deferred: SMART, second invocation, auto-import, generated overrides/case clauses, arbitrary insert handlers, resolve, snippets, " +
      "Java binary short-name enumeration, and IDE statistics/ML parity. Plain lookup edits and trivial named-argument assignments are supported."
    lines += ""
    lines += "## Cases"
    lines += ""
    lines += "| Suite / case | Outcome | Reason |"
    lines += "|---|---|---|"
    def cell(s: String) = s.replace("|", "\\|").replace("\n", " ")
    for ((c, r) <- results) lines += s"| ${c.suite}/${c.name} | ${r.outcome} | ${cell(r.reason)} |"
    lines += ""
    lines += "## Adapter observations"
    lines += ""
    for ((entry, count) <- CallLog.snapshot if entry.startsWith("UNSUPPORTED ") || entry.startsWith("LOGGED ERROR ") || entry.startsWith("Completion.filtered."))
      lines += s"- $entry: $count"
    val report = Path.of(reportPath)
    Files.createDirectories(report.toAbsolutePath.getParent)
    Files.write(report, lines.result().asJava)
    for (suite <- Seq("completion", "keywordCompletion")) {
      val outcomes = results.filter(_._1.suite == suite).map(_._2.outcome)
      println(s"$suite: ${outcomes.count(_ == "agree")} agree, ${outcomes.count(_ == "disagree")} disagree, ${outcomes.count(_ == "skip")} skip")
    }
    println(s"completion oracle: report at $reportPath")
  }

  private def collect(host: ScalaPsiHost, root: Path): Vector[TestCase] = {
    val testRoot = root.resolve("test/org/jetbrains/plugins/scala/lang/completion")
    Vector("completion" -> testRoot.resolve("generated"), "keywordCompletion" -> testRoot.resolve("keyword/generated")).flatMap { (suite, dir) =>
      val sources = Using.resource(Files.list(dir))(_.iterator().asScala.filter(_.toString.endsWith(".scala")).toVector.sorted)
      sources.flatMap { source =>
        val file = host.parse(Files.readString(source))
        PsiTreeUtil.findChildrenOfType(file, classOf[ScClass]).asScala.toVector.flatMap { cls =>
          val folders = cls.functions.collect { case f: ScFunctionDefinition if f.name == "folderPath" => f }
            .flatMap(f => PsiTreeUtil.findChildrenOfType(f, classOf[ScLiteral]).asScala.map(_.getValue.toString))
          cls.functions.collect {
            case f: ScFunctionDefinition if f.name.startsWith("test") =>
              val name = f.name.stripPrefix("test")
              val fileName = if (suite == "keywordCompletion") name.head.toLower + name.tail else name
              val path = folders.foldLeft(root.resolve(s"testdata/$suite"))(_.resolve(_)).resolve(fileName + ".scala")
              val reason = if (cls.name.contains("Smart")) Some("SMART completion is out of scope")
                else if (!f.body.exists(_.getText == "doTest()")) Some("custom test setup is not replayed")
                else None
              TestCase(suite, name, path, reason)
          }
        }
      }
    }
  }

  private def runCase(host: ScalaPsiHost, module: HostModule, c: TestCase): Result = {
    val marker = if (c.suite == "completion") "/*caret*/" else "<caret>"
    try {
      val text = Files.readString(c.path).trim.replace("\r\n", "\n")
      val offset = text.indexOf(marker)
      if (offset < 0 || text.indexOf(marker, offset + marker.length) >= 0) return Result("skip", "requires exactly one caret marker")
      // The fixture opens a physical file. Synthetic ScalaPsiElementFactory files have empty backing VFS text,
      // which changes findReferenceAt and therefore the plugin's operator/backtick dummy selection.
      val configured = if (c.suite == "completion") text else text.replace(marker, "")
      val file = host.openFile(Path.of("/completion-oracle").resolve(c.path.getFileName), configured, module) match {
        case f: org.jetbrains.plugins.scala.lang.psi.api.ScalaFile => f
        case other => throw new IllegalStateException(s"not a Scala fixture: $other")
      }
      try host.read {
        Expected.lastComment(file) match {
          case None => Result("skip", "no last-comment expectation")
          case Some(comment) =>
            val expected = Expected.forVersion(comment, LatestScalaVersions.Scala_2_13).trim.linesIterator.filter(_.nonEmpty).toVector.sorted
            val completed = Completion.complete(file, offset, Int.MaxValue)
            val items = completed.getItems.asScala.filter(i => c.suite != "keywordCompletion" || i.getKind == CompletionItemKind.Keyword)
            val actual = items.map { i =>
              val strikeout = Option(i.getTags).exists(_.contains(CompletionItemTag.Deprecated))
              i.getFilterText + (if (strikeout && c.suite == "completion") " (strikeout)" else "")
            }.toVector.sorted
            if (expected == actual) Result("agree", "")
            else Result("disagree", s"missing: ${expected.diff(actual).mkString(", ")}; extra: ${actual.diff(expected).mkString(", ")}")
        }
      } finally host.closeSource(file)
    } catch {
      case NonFatal(e) =>
        HostLog.error(s"completion oracle ${c.suite}/${c.name} failed", e)
        Result("disagree", s"${e.getClass.getSimpleName}: ${e.getMessage}")
    }
  }
}
