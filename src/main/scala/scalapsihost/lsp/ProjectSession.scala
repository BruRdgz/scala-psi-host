package scalapsihost.lsp

import com.intellij.codeInsight.highlighting.ReadWriteAccessDetector
import com.intellij.ide.util.treeView.smartTree.TreeElement
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.editor.Document
import com.intellij.openapi.util.TextRange
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile, PsiNameIdentifierOwner, PsiNamedElement, SyntaxTraverser}
import org.eclipse.lsp4j.SymbolKind
import org.jetbrains.plugins.scala.editor.documentationProvider.ScalaDocQuickInfoGenerator
import org.jetbrains.plugins.scala.lang.folding.ScalaFoldingBuilder
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScBlock
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScEnumCase, ScExtension, ScFunction, ScTypeAlias, ScVariable}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScEnum, ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.structureView.element.Element
import scalapsihost.*

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

/** A problem in a file, from the plugin (`scala-plugin`) or the compiler (`scalac`). */
final case class FileProblem(severity: HighlightSeverity, start: LineColumn, end: LineColumn, message: String, source: String)

/** An occurrence of a symbol in a file: its range, and whether it writes the symbol (a declaration with a value, an assignment). */
final case class Occurrence(range: (LineColumn, LineColumn), write: Boolean)

/**
 * A declaration in a file's outline: its name, the rest of what the structure view shows (parameters, type), its
 * whole range and its name's.
 */
final case class OutlineSymbol(
  name: String, detail: String, kind: SymbolKind, range: (LineColumn, LineColumn), nameRange: (LineColumn, LineColumn),
  children: Seq[OutlineSymbol],
)

object OutlineSymbol {
  def kind(declaration: PsiElement): SymbolKind = declaration match {
    case _: ScEnumCase => SymbolKind.EnumMember
    case _: ScEnum => SymbolKind.Enum
    case _: ScTrait => SymbolKind.Interface
    case _: ScObject => SymbolKind.Module
    case _: ScTypeDefinition => SymbolKind.Class
    case f: ScFunction if f.isConstructor => SymbolKind.Constructor
    case f: ScFunction => if (f.containingClass == null) SymbolKind.Function else SymbolKind.Method
    case _: ScClassParameter => SymbolKind.Property
    // LSP has no kind for type aliases; editors show a class for them.
    case _: ScTypeAlias => SymbolKind.Class
    case _: ScExtension => SymbolKind.Namespace
    case named: ScNamedElement => named.nameContext match {
      case _: ScVariable => SymbolKind.Variable
      case _ => SymbolKind.Constant
    }
    case _ => SymbolKind.Object
  }
}

/** A folding region: the lines it hides after its first one, up to `endLine`. */
final case class Fold(startLine: Int, endLine: Int, kind: Fold.Kind)

object Fold {
  enum Kind { case Code, Comment, Imports, Region }
  export Kind.*
}

/**
 * An open project: its build (kept connected, to compile on save and to import again after a build change), its
 * modules in the host and every source file of them, loaded from disk and then kept in step with the editor's
 * changes. Positions are 0-based lines and columns in UTF-16 code units, as LSP counts them and as documents index
 * their text.
 */
final class ProjectSession private (
  host: ScalaPsiHost,
  build: BuildConnection,
) extends AutoCloseable {
  private val files = new ConcurrentHashMap[Path, PsiFile]()
  // The build's modules as last imported (changed only by `importBuild`, on the session's thread).
  @volatile private var imported: Seq[ImportedModule] = Seq.empty
  @volatile private var compileSpecs: Map[String, CompileSpec] = Map.empty
  // Classpath jars whose sources were asked for, attached or not.
  private var sourcesAskedFor = Set.empty[Path]
  private val documentCompiler = new DocumentCompiler
  // The build's compiler problems from its last compile, by file (for files the document compiler does not cover).
  private val buildProblems = new AtomicReference[Map[Path, Seq[CompilerDiagnostic]]](Map.empty)

  def sourceFiles: Seq[Path] = files.keySet.toArray(Array.empty[Path]).toSeq

  /** Whether `path` is a source file of the project (not, say, a library file the editor opened). */
  def isSource(path: Path): Boolean = files.containsKey(path)

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

  /** Scala completion preserves the file's module context and uses the host's cancellable read. */
  def completion(path: Path, at: LineColumn): org.eclipse.lsp4j.CompletionList = host.read {
    file(path) match {
      case f: ScalaFile => host.completion(f, offset(document(f), at))
      case _ => new org.eclipse.lsp4j.CompletionList(false, java.util.List.of())
    }
  }

  /** Copy modifications acquire their own write scope after positions have been read. */
  def formatting(path: Path, range: Option[(LineColumn, LineColumn)], options: org.eclipse.lsp4j.FormattingOptions)
      : Seq[org.eclipse.lsp4j.TextEdit] = file(path) match {
    case f: ScalaFile =>
      val selected = host.read {
        range.map((start, end) => new TextRange(offset(document(f), start), offset(document(f), end)))
      }
      host.formatting(f, selected, options)
    case _ => Seq.empty
  }

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
    val atOffset = offset(document(f), at)
    // The referenced symbol, or the one declared at `at` when the cursor is on a declaration's name.
    val subject = Option(f.findReferenceAt(atOffset)) match {
      case Some(reference) => Option(reference.resolve()).map(_ -> reference.getElement)
      case None => declarationNamedAt(f, atOffset).map(declaration => declaration -> declaration)
    }
    for {
      (target, original) <- subject
      html <- ScalaDocQuickInfoGenerator.getQuickNavigateInfo(target, original)
    } yield StringUtil.unescapeXmlEntities(StringUtil.removeHtmlTags(html, true)).trim
  }

  private def declarationNamedAt(file: PsiFile, at: Int): Option[PsiNameIdentifierOwner] =
    Option(file.findElementAt(at)).flatMap { leaf =>
      Option(com.intellij.psi.util.PsiTreeUtil.getParentOfType(leaf, classOf[PsiNameIdentifierOwner]))
        .filter(owner => owner.getNameIdentifier != null && owner.getNameIdentifier.getTextRange.contains(at))
    }

  /**
   * Where the symbol referenced at `at` is declared: its file and the position of its name. Only declarations in
   * the project's sources; library declarations have no file to open yet.
   */
  def definition(path: Path, at: LineColumn): Option[(Path, LineColumn)] = host.read {
    val f = file(path)
    for {
      reference <- Option(f.findReferenceAt(offset(document(f), at)))
      target <- Option(reference.resolve())
      location <- sourceLocation(target)
    } yield location
  }

  /** Where the type of the symbol at `at` is declared, as the IDE's "go to type declaration" (project sources only). */
  def typeDefinition(path: Path, at: LineColumn): Seq[(Path, LineColumn)] = host.read {
    val f = file(path)
    val atOffset = offset(document(f), at)
    val symbol = Option(f.findReferenceAt(atOffset)).flatMap(r => Option(r.resolve())).orElse(declarationNamedAt(f, atOffset))
    val types = symbol.flatMap(s => Option(new org.jetbrains.plugins.scala.annotator.gutter.ScalaGotoTypeDeclarationProvider().getSymbolTypeDeclarations(s)))
    types.toSeq.flatten.flatMap(sourceLocation)
  }

  /**
   * A declaration's file and the position of its name. A library declaration is in a file of the host's state
   * directory (see [[libraryFileOnDisk]]): its library's attached source, found as the IDE finds it (the declaration's
   * navigation element), or else its decompiled class file, as the IDE shows a library without sources.
   */
  private def sourceLocation(declaration: PsiElement): Option[(Path, LineColumn)] = {
    val target = declaration.getNavigationElement
    val nameOffset = target match {
      case named: PsiNameIdentifierOwner if named.getNameIdentifier != null => named.getNameIdentifier.getTextOffset
      case other => other.getTextOffset
    }
    for {
      targetFile <- Option(target.getContainingFile)
      location <- targetFile.getViewProvider.getVirtualFile match {
        case source: SourceFile => Some(source.path -> position(document(targetFile), nameOffset))
        case library: LibraryFile =>
          val text = targetFile.getText
          val at = StringUtil.offsetToLineColumn(text, nameOffset)
          Some(libraryFileOnDisk(library, targetFile, text) -> LineColumn(at.line, at.column))
        case _ => None
      }
    } yield location
  }

  /**
   * A library file's text written under the state directory, at its path inside its library (by root, so files
   * of different jars stay apart): editors open plain files, not entries of jars. A class file is written as the
   * source the IDE decompiles it to (`.scala`, or `.java` for Java classes). Written read-only, and again only when
   * its text changed (a snapshot jar replaced in place).
   */
  private def libraryFileOnDisk(file: LibraryFile, psi: PsiFile, text: String): Path = {
    val rootName = file.root.getName
    val rootDirectory = s"${rootName.substring(rootName.lastIndexOf('/') + 1)}-${Integer.toHexString(rootName.hashCode)}"
    val entry = if (file.isLibrarySource) file.entry else {
      val extension = if (psi.isInstanceOf[ScalaFile]) ".scala" else ".java"
      file.entry.substring(0, file.entry.lastIndexOf('.')) + extension
    }
    val onDisk = host.stateDir.resolve("library-files").resolve(rootDirectory).resolve(entry)
    if (!Files.exists(onDisk) || Files.readString(onDisk) != text) {
      Files.createDirectories(onDisk.getParent)
      onDisk.toFile.setWritable(true)
      Files.writeString(onDisk, text)
      onDisk.toFile.setReadOnly()
    }
    onDisk
  }

  /**
   * The occurrences in this file of the symbol at `at` (referenced or declared there), as the IDE highlights them
   * under the caret: its declaration and the references to it, read or written as the plugin's access detector says.
   */
  def highlights(path: Path, at: LineColumn): Seq[Occurrence] = host.read {
    val f = file(path)
    val d = document(f)
    val atOffset = offset(d, at)
    val target = Option(f.findReferenceAt(atOffset)) match {
      case Some(reference) => Option(reference.resolve())
      case None => declarationNamedAt(f, atOffset)
    }
    target.collect { case named: PsiNamedElement if named.getName != null => named }.toSeq.flatMap { named =>
      val access = new org.jetbrains.plugins.scala.highlighter.readWriteAccess.ScalaReadWriteAccessDetector
      def span(range: TextRange) = position(d, range.getStartOffset) -> position(d, range.getEndOffset)
      val declaration = named.getNavigationElement match {
        case owner: PsiNameIdentifierOwner if owner.getContainingFile == f && owner.getNameIdentifier != null =>
          Seq(Occurrence(span(owner.getNameIdentifier.getTextRange), write = access.isDeclarationWriteAccess(owner)))
        case _ => Seq.empty
      }
      // Only references whose text names the symbol can refer to it (the IDE's search also looks for the name's
      // occurrences first); resolving every reference of the file would be the whole highlighting pass.
      val references = SyntaxTraverser.psiTraverser(f).asScala.toSeq.flatMap(_.getReferences.toSeq).filter { reference =>
        val text = reference.getRangeInElement.substring(reference.getElement.getText)
        ScalaNamesUtil.equivalent(text, named.getName) && reference.isReferenceTo(named)
      }
      declaration ++ references.map { reference =>
        val write = access.getReferenceAccess(named, reference) == ReadWriteAccessDetector.Access.Write
        Occurrence(span(reference.getRangeInElement.shiftRight(reference.getElement.getTextRange.getStartOffset)), write)
      }
    }.distinct
  }

  /** The file's outline, as the IDE's structure view shows it (Scala files; Java sources have none). */
  def outline(path: Path): Seq[OutlineSymbol] = host.read {
    file(path) match {
      case scalaFile: ScalaFile =>
        val d = document(scalaFile)
        def span(range: TextRange) = position(d, range.getStartOffset) -> position(d, range.getEndOffset)
        def symbols(node: TreeElement): Seq[OutlineSymbol] = node match {
          case element: Element =>
            val children = element.getChildren.toSeq.flatMap(symbols)
            element.element match {
              // An initializer block has no name: its declarations are shown under the enclosing one.
              case _: ScBlock => children
              case psi =>
                val text = element.getPresentation.getPresentableText
                val (name, detail) = psi match {
                  case named: ScNamedElement if text.startsWith(named.name) => named.name -> text.drop(named.name.length)
                  case _ => text -> ""
                }
                // A val's or var's binding is the element; its definition is the whole statement.
                val whole = psi match {
                  case named: ScNamedElement => named.nameContext
                  case other => other
                }
                val nameRange = psi match {
                  case named: ScNamedElement => named.nameId.getTextRange
                  case other => TextRange.from(other.getTextOffset, 0)
                }
                Seq(OutlineSymbol(name, detail, OutlineSymbol.kind(psi), span(whole.getTextRange), span(nameRange), children))
            }
          case _ => Seq.empty
        }
        Element(() => scalaFile).getChildren.toSeq.flatMap(symbols)
      case _ => Seq.empty
    }
  }

  /**
   * The file's folding regions, as the plugin's folding builder computes them for the IDE's editor. A region that
   * ends on a closing bracket leaves that bracket's line unfolded.
   */
  def foldingRanges(path: Path): Seq[Fold] = host.read {
    file(path) match {
      case scalaFile: ScalaFile =>
        val d = document(scalaFile)
        val text = d.getCharsSequence
        new ScalaFoldingBuilder().buildFoldRegions(scalaFile, d, /* quick = */ false).toSeq.flatMap { descriptor =>
          val range = descriptor.getRange
          val startLine = d.getLineNumber(range.getStartOffset)
          val lastLine = d.getLineNumber(range.getEndOffset)
          val closes = range.getLength > 0 && "})]".contains(text.charAt(range.getEndOffset - 1))
          val endLine = if (closes && lastLine > startLine) lastLine - 1 else lastLine
          val node = descriptor.getElement
          val kind = node.getElementType match {
            case t if org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes.COMMENTS_TOKEN_SET.contains(t) =>
              if (com.intellij.lang.folding.CustomFoldingBuilder.isCustomRegionElement(node.getPsi)) Fold.Region else Fold.Comment
            case t if t == org.jetbrains.plugins.scala.lang.scaladoc.parser.ScalaDocElementTypes.SCALA_DOC_COMMENT => Fold.Comment
            case t if t == org.jetbrains.plugins.scala.lang.parser.ScalaElementType.ImportStatement => Fold.Imports
            case _ => Fold.Code
          }
          Option.when(endLine > startLine)(Fold(startLine, endLine, kind))
        }.distinct
      case _ => Seq.empty
    }
  }

  /** The file's identifiers and soft keywords colored as the IDE colors them (see [[ScalaPsiHost.textAttributes]]). */
  def semanticTokens(path: Path): Seq[SemanticColoring.Token] = file(path) match {
    case scalaFile: ScalaFile =>
      host.read {
        val d = document(scalaFile)
        host.textAttributes(scalaFile).collect {
          case (range, key) if !range.isEmpty && d.getLineNumber(range.getStartOffset) == d.getLineNumber(range.getEndOffset) =>
            SemanticColoring.Token(position(d, range.getStartOffset), range.getLength, key)
        }
      }
    case _ => Seq.empty
  }

  /**
   * Imports the build's modules and applies them to the host: on opening, every module and its sources; after a
   * build change, the difference. Sources the build added are opened (from disk) and those it removed closed; a
   * module whose definition changed (Scala version, options, classpath, dependencies), and every module that
   * depends on it, is removed and added again, its files reopened with their current text (unsaved editor changes
   * are kept). `reload` first reloads the build's definition (after a change to it). Each step is logged, and
   * reported to `progress` as it starts.
   */
  def importBuild(reload: Boolean, progress: String => Unit = _ => ()): Unit = {
    val start = System.nanoTime()
    if (reload) {
      progress("reloading the build definition")
      HostLog.timed("build definition reloaded")(build.reload())
    }
    progress("importing the build's modules")
    val next = HostLog.timed("modules described by the build")(build.modules())
    val before = imported.map(m => m.spec.name -> m).toMap
    val after = next.map(m => m.spec.name -> m).toMap
    val changed = before.keySet.filter(name => !after.get(name).exists(_.spec == before(name).spec))
    // A module's dependents see it through their scopes: they are added again with it.
    def dependents(names: Set[String]): Set[String] = {
      val more = names ++ before.values.filter(_.spec.dependsOn.exists(names)).map(_.spec.name)
      if (more == names) names else dependents(more)
    }
    val replaced = dependents(changed)
    // Removed dependents first (reverse of the dependencies-first import order), keeping their files' text.
    val reopened = scala.collection.mutable.Map.empty[Path, String]
    for (m <- imported.reverse if replaced(m.spec.name)) {
      for (source <- m.sources; file <- Option(files.remove(source))) {
        reopened(source) = host.read(file.getText)
        host.closeSource(file)
      }
      host.removeModule(m.spec.name)
    }
    var added, opened, closed = 0
    for (m <- next) {
      if (!before.contains(m.spec.name) || replaced(m.spec.name)) {
        progress(s"module ${m.spec.name}: ${m.spec.classpath.size} jars, ${m.sources.size} sources")
        val module = HostLog.timed(s"module ${m.spec.name} (Scala ${m.spec.scalaVersion}, ${m.spec.classpath.size} jars) added")(
          host.addModule(m.spec))
        for (source <- m.sources) open(source, reopened.getOrElse(source, Files.readString(source)), module)
        added += 1
        opened += m.sources.size
      } else {
        val module = host.modules.named(m.spec.name).get
        val (kept, now) = (before(m.spec.name).sources.toSet, m.sources.toSet)
        for (source <- kept -- now; file <- Option(files.remove(source))) { host.closeSource(file); closed += 1 }
        for (source <- now -- kept) { open(source, Files.readString(source), module); opened += 1 }
      }
    }
    val removed = before.keySet -- after.keySet
    imported = next
    compileSpecs = next.map(m => m.spec.name -> m.compile).toMap
    // Library sources, as the IDE attaches them on import: the build's source jars (for jars not asked for before).
    val classpath = next.flatMap(_.spec.classpath).toSet -- sourcesAskedFor
    if (classpath.nonEmpty) {
      progress(s"attaching library sources (${classpath.size} jars)")
      val sources = HostLog.timed(s"source jars asked for ${classpath.size} jars")(build.librarySources(classpath))
      for ((jar, jarSources) <- sources) host.addLibrarySources(jar, jarSources)
      HostLog.info(s"library sources attached to ${sources.size} of ${classpath.size} jars")
    }
    sourcesAskedFor ++= classpath
    HostLog.info(s"build imported in ${(System.nanoTime() - start) / 1_000_000} ms: ${next.size} modules " +
      s"(${added - (replaced -- removed).size} new, ${(replaced -- removed).size} added again, ${removed.size} removed), " +
      s"$opened sources opened, $closed closed, ${files.size} in all")
  }

  /** How many modules the build has (as last imported). */
  def moduleCount: Int = imported.size

  private def open(source: Path, text: String, module: HostModule): Unit = files.put(source, host.openFile(source, text, module))

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
  /**
   * Imports the build at `root` over BSP, adds its modules to `host` and opens all their sources from disk. Steps
   * are logged and reported to `progress` as they start.
   */
  def open(host: ScalaPsiHost, root: Path, connection: Option[String] = None, progress: String => Unit = _ => ()): ProjectSession = {
    progress("connecting to the build server")
    val build = HostLog.timed(s"build server of $root connected")(BspImport.connect(root, connection))
    try {
      val javaHome = Path.of(System.getProperty("java.home"))
      host.addJdk(javaHome)
      // The JDK's sources, as the IDE attaches them to the JDK.
      Some(javaHome.resolve("lib/src.zip")).filter(Files.isRegularFile(_)).foreach(host.addJdkSources)
      val session = new ProjectSession(host, build)
      session.importBuild(reload = false, progress)
      session
    } catch {
      case e: Throwable =>
        build.close()
        throw e
    }
  }
}
