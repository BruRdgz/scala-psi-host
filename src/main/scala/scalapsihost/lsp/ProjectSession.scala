package scalapsihost.lsp

import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.editor.Document
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.{PsiDocumentManager, PsiFile, PsiNameIdentifierOwner}
import org.jetbrains.plugins.scala.editor.documentationProvider.ScalaDocQuickInfoGenerator
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import scalapsihost.*

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

/** A problem in a file, from the plugin (`scala-plugin`) or the compiler (`scalac`). */
final case class FileProblem(severity: HighlightSeverity, start: LineColumn, end: LineColumn, message: String, source: String)

/**
 * An open project: its build (kept connected, to compile on save), its modules in the host and every source file
 * of them, loaded from disk and then kept in step with the editor's changes. Positions are 0-based lines and
 * columns in UTF-16 code units, as LSP counts them and as documents index their text.
 */
final class ProjectSession private (
  host: ScalaPsiHost,
  build: BuildConnection,
  files: ConcurrentHashMap[Path, PsiFile],
  compileSpecs: Map[String, CompileSpec],
) extends AutoCloseable {
  private val documentCompiler = new DocumentCompiler
  // The build's compiler problems from its last compile, by file (for files the document compiler does not cover).
  private val buildProblems = new AtomicReference[Map[Path, Seq[CompilerDiagnostic]]](Map.empty)

  def sourceFiles: Seq[Path] = files.keySet.toArray(Array.empty[Path]).toSeq

  private def file(path: Path): PsiFile =
    Option(files.get(path)).getOrElse(throw new IllegalArgumentException(s"not a source file of the project: $path"))

  private def document(file: PsiFile): Document = PsiDocumentManager.getInstance(host.project).getDocument(file)

  private def offset(document: Document, position: LineColumn): Int =
    if (position.line >= document.getLineCount) document.getTextLength
    else math.min(document.getLineStartOffset(position.line) + position.column, document.getLineEndOffset(position.line))

  private def position(document: Document, offset: Int): LineColumn = {
    val line = document.getLineNumber(offset)
    LineColumn(line, offset - document.getLineStartOffset(line))
  }

  def text(path: Path): String = host.read(file(path).getText)

  /**
   * Applies an editor change: the text between `start` and `end` becomes `replacement`; without a range, the whole
   * text is replaced.
   */
  def change(path: Path, range: Option[(LineColumn, LineColumn)], replacement: String): Unit = {
    val f = file(path)
    val (start, end) = host.read {
      val d = document(f)
      range.fold((0, d.getTextLength))((from, to) => (offset(d, from), offset(d, to)))
    }
    host.edit(f, start, end, replacement)
  }

  /**
   * The file's problems: the plugin's, and for files whose errors come from the compiler (Scala 3 by default), the
   * compiler's: of the current text for a Scala 3 module, of the build's last compile otherwise.
   */
  def problems(path: Path): Seq[FileProblem] = file(path) match {
    case scalaFile: ScalaFile =>
      val (fromPlugin, compilerHighlighted, text) = host.read {
        val d = document(scalaFile)
        val plugin = host.diagnostics(scalaFile).filter(_.severity.compareTo(HighlightSeverity.WEAK_WARNING) >= 0).map { p =>
          FileProblem(p.severity, position(d, p.range.getStartOffset), position(d, p.range.getEndOffset), p.message, "scala-plugin")
        }
        (plugin, host.isCompilerHighlighted(scalaFile), d.getText)
      }
      val fromCompiler =
        if (!compilerHighlighted) Seq.empty
        else {
          val spec = moduleSpec(scalaFile)
          val reported =
            if (spec.exists(_.scalaVersion.startsWith("3."))) documentCompiler.compile(spec.get, path.getFileName.toString, text)
            else buildProblems.get.getOrElse(path, Seq.empty)
          reported.map(c => FileProblem(c.severity, c.start, c.end, c.message, "scalac"))
        }
      fromPlugin ++ fromCompiler
    case _ => Seq.empty // Java sources are only there to be resolved against.
  }

  private def moduleSpec(file: PsiFile): Option[CompileSpec] =
    host.modules.ofFile(file.getViewProvider.getVirtualFile).flatMap(m => compileSpecs.get(m.spec.name))

  /**
   * The signature of the symbol referenced at `at`, as the IDE's quick documentation shows it: the plugin's quick
   * info, without the editor's hint styling (ScalaDocumentationProvider adds that for its Swing popup).
   */
  def hover(path: Path, at: LineColumn): Option[String] = host.read {
    val f = file(path)
    for {
      reference <- Option(f.findReferenceAt(offset(document(f), at)))
      target <- Option(reference.resolve())
      html <- ScalaDocQuickInfoGenerator.getQuickNavigateInfo(target, reference.getElement)
    } yield StringUtil.unescapeXmlEntities(StringUtil.removeHtmlTags(html, true)).trim
  }

  /**
   * Where the symbol referenced at `at` is declared: its file and the position of its name. Only declarations in
   * the project's sources; library declarations have no file to open yet.
   */
  def definition(path: Path, at: LineColumn): Option[(Path, LineColumn)] = host.read {
    val f = file(path)
    for {
      reference <- Option(f.findReferenceAt(offset(document(f), at)))
      target <- Option(reference.resolve()).map(_.getNavigationElement)
      targetFile <- Option(target.getContainingFile)
      source <- Option(targetFile.getViewProvider.getVirtualFile).collect { case s: SourceFile => s }
    } yield {
      val nameOffset = target match {
        case named: PsiNameIdentifierOwner if named.getNameIdentifier != null => named.getNameIdentifier.getTextOffset
        case other => other.getTextOffset
      }
      source.path -> position(document(targetFile), nameOffset)
    }
  }

  /** After a save: compiles the build and keeps its problems; returns the files whose compiler problems changed. */
  def compileBuild(): Set[Path] = {
    val before = buildProblems.get
    val after = build.compile().diagnostics
    buildProblems.set(after)
    (before.keySet ++ after.keySet).filter(p => before.get(p) != after.get(p) && files.containsKey(p))
  }

  override def close(): Unit = build.close()
}

object ProjectSession {
  /** Imports the build at `root` over BSP, adds its modules to `host` and opens all their sources from disk. */
  def open(host: ScalaPsiHost, root: Path, connection: Option[String] = None): ProjectSession = {
    val build = BspImport.connect(root, connection)
    try {
      val imported = build.modules()
      host.addJdk(Path.of(System.getProperty("java.home")))
      val files = new ConcurrentHashMap[Path, PsiFile]()
      for (m <- imported) {
        val module = host.addModule(m.spec)
        for (source <- m.sources) files.put(source, host.openFile(source, Files.readString(source), module))
      }
      new ProjectSession(host, build, files, imported.map(m => m.spec.name -> m.compile).toMap)
    } catch {
      case e: Throwable =>
        build.close()
        throw e
    }
  }
}
