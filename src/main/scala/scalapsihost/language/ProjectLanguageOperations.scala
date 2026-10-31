package scalapsihost.language

import com.intellij.codeInsight.highlighting.ReadWriteAccessDetector
import com.intellij.ide.util.treeView.smartTree.TreeElement
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.util.TextRange
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile, PsiNameIdentifierOwner, PsiNamedElement, SyntaxTraverser}
import org.jetbrains.plugins.scala.extensions.PsiNamedElementExt
import org.jetbrains.plugins.scala.editor.documentationProvider.{ScalaDocQuickInfoGenerator, ScalaDocumentationProvider}
import org.jetbrains.plugins.scala.highlighter.DefaultHighlighter as H
import org.jetbrains.plugins.scala.lang.folding.ScalaFoldingBuilder
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScBlock
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScEnumCase, ScExtension, ScFunction, ScTypeAlias, ScVariable}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScEnum, ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.structureView.element.Element

import scalapsihost.{LineColumn, ScalaPsiHost}
import scalapsihost.build.{CompileSpec, CompilerDiagnostic, DocumentCompiler, ScalafmtFormatting}
import scalapsihost.platform.{HostLog, LibraryFile, SourceFile}
import scalapsihost.*

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** PSI-backed navigation, document operations and language results for a project session. */
private[scalapsihost] final class ProjectLanguageOperations(
  host: ScalaPsiHost,
  root: Path,
  documentCompiler: DocumentCompiler,
  scalafmt: ScalafmtFormatting,
  file: Path => PsiFile,
) {
  /** Gets the in-memory document owned by the platform for `file`. */
  private def document(file: PsiFile): Document = PsiDocumentManager.getInstance(host.project).getDocument(file)

  /** Converts a workspace line/UTF-16 column to a document offset, clamping positions past a line's end. */
  private def offset(document: Document, position: LineColumn): Int =
    if (position.line >= document.getLineCount) document.getTextLength
    else math.min(document.getLineStartOffset(position.line) + position.column, document.getLineEndOffset(position.line))

  /** Converts a document offset to its zero-based line and UTF-16 column. */
  private def position(document: Document, offset: Int): LineColumn = {
    val line = document.getLineNumber(offset)
    LineColumn(line, offset - document.getLineStartOffset(line))
  }

  /** The current in-memory document text, including changes not yet saved to disk. */
  def text(path: Path): String = host.read(file(path).getText)

  /** The completion suggestions at `at`, best first. Only Scala files complete. */
  def completion(path: Path, at: LineColumn): CompletionChoices = file(path) match {
    case f: ScalaFile =>
      host.read {
        val text = document(f)
        val suggestions = host.completion(f, offset(text, at))
        val choices = suggestions.items.map { suggestion =>
          val candidate = CompletionCandidate(suggestion.label, suggestion.tail, suggestion.typeText, suggestion.kind,
            suggestion.deprecated, suggestion.lookup, suggestion.prefixText, suggestion.hasHandler, suggestion.identity)
          CompletionChoice(candidate, position(text, suggestion.prefix.getStartOffset), position(text, suggestion.prefix.getEndOffset))
        }
        CompletionChoices(choices, suggestions.incomplete)
      }
    case _ => CompletionChoices(Seq.empty, incomplete = false)
  }

  /** The plugin's inline hints within the requested range. Only Scala files have them. */
  def inlayHints(path: Path, start: LineColumn, end: LineColumn): Seq[InlineHint] = file(path) match {
    case f: ScalaFile => host.read {
      val d = document(f)
      val range = new TextRange(offset(d, start), offset(d, end))
      host.inlayHints(f).filter(hint => range.contains(hint.offset)).map(hint => InlineHint(position(d, hint.offset), hint.text))
    }
    case _ => Seq.empty
  }

  /** The signatures of the call around `at`. Only Scala files have them. */
  def signatures(path: Path, at: LineColumn): Option[SignatureChoices] = file(path) match {
    case f: ScalaFile => host.read(host.parameterInfo(f, offset(document(f), at))).map { found =>
      SignatureChoices(found.items.map { signature =>
        SignatureInfo(signature.text, signature.current.map(r => OffsetRange(r.getStartOffset, r.getEndOffset)),
          signature.applicable, signature.deprecated)
      }, found.active)
    }
    case _ => None
  }

  /** Each position's Extend Selection chain. An unexpandable caret still has its empty range, as LSP requires. */
  def selectionRanges(path: Path, positions: Seq[LineColumn]): Seq[Seq[(LineColumn, LineColumn)]] = file(path) match {
    case f: ScalaFile => host.read {
      val d = document(f)
      positions.map { at =>
        val cursor = offset(d, at)
        val ranges = host.selectionRanges(f, cursor)
        val chain = if (ranges.isEmpty) Seq(new TextRange(cursor, cursor)) else ranges
        chain.map(r => position(d, r.getStartOffset) -> position(d, r.getEndOffset))
      }
    }
    case _ => Seq.empty
  }

  /**
   * Runs the insert handler of the suggestion `identity`, whose lookup string `lookup` the editor has put at `start`
   * in place of `prefixText` (see [[Completion.insert]]). None when the file no longer has it there.
   */
  def insertCompletion(path: Path, start: LineColumn, prefixText: String, lookup: String, identity: String)
      : Option[CompletionFollowUp] =
    file(path) match {
      case f: ScalaFile =>
        val startOffset = host.read(offset(document(f), start))
        host.insertCompletion(f, startOffset, prefixText, lookup, identity).map { inserted =>
          val changes = host.read(inserted.result.edits.map(textChange(document(f), _)))
          val caret = StringUtil.offsetToLineColumn(inserted.result.text, inserted.caret)
          CompletionFollowUp(changes, LineColumn(caret.line, caret.column))
        }
      case _ => None
    }

  /** The changes that format `range` of the file (all of it without one). Only Scala files are formatted. */
  def formatting(path: Path, range: Option[(LineColumn, LineColumn)], indentSize: Int, useTabs: Boolean): Seq[TextChange] =
    file(path) match {
      case f: ScalaFile =>
        val selected = host.read(range.map((start, end) => new TextRange(offset(document(f), start), offset(document(f), end))))
        // The plugin defaults to the IntelliJ formatter for range requests even when scalafmt is selected.
        val config = if (range.isEmpty) ScalafmtFormatting.configFor(root, path) else None
        val edits = config match {
          case Some(configFile) =>
            val before = host.read(f.getText)
            ChangeEngine.diff(before, scalafmt.format(configFile, path, before))
          case None => host.formatting(f, selected, indentSize, useTabs)
        }
        host.read(edits.map(textChange(document(f), _)))
      case _ => Seq.empty
    }

  /** Converts a PSI text edit's document offsets to a workspace text change. */
  private def textChange(document: Document, edit: ChangeEngine.Edit): TextChange =
    TextChange(position(document, edit.range.getStartOffset), position(document, edit.range.getEndOffset), edit.text)

  /**
   * Applies an editor change: the text between `start` and `end` becomes `replacement`; without a range, the whole
   * text is replaced.
   */
  def change(path: Path, range: Option[(LineColumn, LineColumn)], replacement: String): Unit = {
    val f = file(path)
    // Only this thread edits documents, so reading one needs no lock; and an edit must not be cancelled as the
    // queries a newer edit cancels are.
    val d = document(f)
    val (start, end) = range.fold((0, d.getTextLength))((from, to) => (offset(d, from), offset(d, to)))
    host.edit(f, start, end, replacement)
  }

  /**
   * The file's problems: the plugin's, and for files whose errors come from the compiler (Scala 3 by default), the
   * compiler's: of the current text for a Scala 3 module, of the build's last compile otherwise.
   */
  def problems(path: Path, spec: Option[CompileSpec], lastBuildProblems: Seq[CompilerDiagnostic]): Seq[FileProblem] =
    file(path) match {
      case scalaFile: ScalaFile =>
        val (fromPlugin, compilerHighlighted, text) = host.read {
          val d = document(scalaFile)
          val plugin = host.diagnostics(scalaFile).filter(_.severity.compareTo(HighlightSeverity.WEAK_WARNING) >= 0).map { p =>
            FileProblem(problemSeverity(p.severity), position(d, p.range.getStartOffset), position(d, p.range.getEndOffset), p.message, "scala-plugin")
          }
          (plugin, host.isCompilerHighlighted(scalaFile), d.getText)
        }
        val fromCompiler =
          if (!compilerHighlighted) Seq.empty
          else {
            val reported = spec match {
              case Some(compileSpec) if compileSpec.scalaVersion.startsWith("3.") =>
                documentCompiler.compile(compileSpec, path.getFileName.toString, text)
              case _ => lastBuildProblems
            }
            reported.map(c => FileProblem(problemSeverity(c.severity), c.start, c.end, c.message, "scalac"))
          }
        fromPlugin ++ fromCompiler
      case _ => Seq.empty // Java sources are only there to be resolved against.
    }

  /**
   * The Markdown the IDE's quick documentation shows for the symbol referenced at `at`: its signature, from the
   * plugin's quick info without the editor's hint styling (ScalaDocumentationProvider adds that for its Swing popup),
   * then its ScalaDoc, if it has any.
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
    } yield {
      val signature = StringUtil.unescapeXmlEntities(StringUtil.removeHtmlTags(html, true)).trim
      (s"```scala\n$signature\n```" +: documentation(target, original).toSeq).mkString("\n\n")
    }
  }

  /**
   * The ScalaDoc of `target` as Markdown; none for a symbol without any, or that is not Scala's (a JDK class). A
   * library symbol's doc is in its attached source: the navigation element, as the IDE documents it.
   */
  private def documentation(target: PsiElement, original: PsiElement): Option[String] =
    try Option(new ScalaDocumentationProvider().generateDoc(target.getNavigationElement, original)).flatMap(QuickDoc.toMarkdown)
    catch {
      // A linkage error: the generator reached an IDE class the host does not ship; the signature is still worth showing.
      case e @ (NonFatal(_) | _: LinkageError) =>
        HostLog.warn(s"no documentation for ${target.getText.take(40)}", e)
        None
    }

  /** Finds a declaration whose name identifier contains `at`, when there is no reference at that offset. */
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

  /** Where a declaration's name is in its file; an empty range at its start for one without a name identifier. */
  private def nameRange(declaration: PsiElement): TextRange = declaration match {
    case named: PsiNameIdentifierOwner if named.getNameIdentifier != null => named.getNameIdentifier.getTextRange
    case other => TextRange.from(other.getTextOffset, 0)
  }

  /** `range` of `file` as a span, if the file is a project source. */
  private def sourceSpan(file: PsiFile, range: TextRange): Option[SourceSpan] = file.getViewProvider.getVirtualFile match {
    case source: SourceFile =>
      val d = document(file)
      Some(SourceSpan(source.path, position(d, range.getStartOffset) -> position(d, range.getEndOffset)))
    case _ => None
  }

  /** The source-file range of a declaration's name, if that declaration belongs to a project source. */
  private def nameSpan(declaration: PsiElement): Option[SourceSpan] =
    Option(declaration.getContainingFile).flatMap(sourceSpan(_, nameRange(declaration)))

  /**
   * A declaration's file and the position of its name. A library declaration is in a file of the host's state
   * directory (see [[libraryFileOnDisk]]): its library's attached source, found as the IDE finds it (the declaration's
   * navigation element), or else its decompiled class file, as the IDE shows a library without sources.
   */
  private def sourceLocation(declaration: PsiElement): Option[(Path, LineColumn)] = {
    val target = declaration.getNavigationElement
    val nameOffset = nameRange(target).getStartOffset
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
   * Where the symbol at `at` is used, in the project's sources: the usages the plugin's find-usages handler reports,
   * and the declaration's name if `includeDeclaration`.
   */
  def references(path: Path, at: LineColumn, includeDeclaration: Boolean): Seq[SourceSpan] = host.read {
    val f = file(path)
    ProjectSearch.usageTarget(f, offset(document(f), at)).toSeq.flatMap { target =>
      val usages = ProjectSearch.usages(target).flatMap(usage => sourceSpan(usage.getFile, TextRange.create(usage.getNavigationRange)))
      val declaration = if (includeDeclaration) nameSpan(target.getNavigationElement) else None
      (declaration.toSeq ++ usages).distinct
    }
  }

  /** Applicable plugin actions, with edits against the current document. */
  def codeActions(path: Path, start: LineColumn, end: LineColumn, only: Seq[String] = Seq.empty)
      : Seq[(CodeActionResult, Seq[TextChange])] = host.read {
    file(path) match {
      case f: ScalaFile =>
        val d = document(f)
        def accepts(kind: String): Boolean = only.isEmpty || only.exists(k => kind == k || kind.startsWith(k + "."))
        val contextual = if (accepts("quickfix") || accepts("refactor.rewrite"))
          host.codeActions(f, new TextRange(offset(d, start), offset(d, end))).filter(choice => accepts(choice.kind))
        else Seq.empty
        val source = if (only.nonEmpty && accepts("source.organizeImports")) Seq(host.organizeImports(f)) else Seq.empty
        val choices = contextual ++ source
        choices.map { choice =>
          CodeActionResult(choice.title, choice.kind, choice.disabled) -> choice.edits.map(textChange(d, _))
        }
      case _ => Seq.empty
    }
  }

  /**
   * The range of the name at `at` that a rename changes: the reference's, or the declaration's name. Refused
   * ([[Rename.Refused]]) where the IDE would not rename: no symbol, a library symbol, a keyword...
   */
  def prepareRename(path: Path, at: LineColumn): (LineColumn, LineColumn) = {
    val f = file(path)
    val cursor = host.read(offset(document(f), at))
    host.renameTarget(f, cursor)
    host.read {
      val reference = Option(f.findReferenceAt(cursor)).map(r => r.getRangeInElement.shiftRight(r.getElement.getTextRange.getStartOffset))
      val name = reference.orElse(declarationNamedAt(f, cursor).map(_.getNameIdentifier.getTextRange))
        .getOrElse(throw new Rename.Refused(Seq("There is no name to rename here.")))
      position(document(f), name.getStartOffset) -> position(document(f), name.getEndOffset)
    }
  }

  /**
   * The changes that rename the symbol at `at` to `newName`, in every project file affected; the files are left as
   * they are, for the editor to apply the changes to and report back. Refused ([[Rename.Refused]]) for what the IDE
   * would not rename, an invalid name or a conflict.
   */
  def rename(path: Path, at: LineColumn, newName: String): Seq[FileChanges] = {
    val f = file(path)
    val edits = host.rename(f, host.read(offset(document(f), at)), newName)
    host.read(edits.flatMap { (changed, fileEdits) =>
      changed.getViewProvider.getVirtualFile match {
        case source: SourceFile => Some(FileChanges(source.path, fileEdits.map(textChange(document(changed), _))))
        case _ => None
      }
    })
  }

  /** The names of the implementations and overriders of the symbol at `at` (project sources only). */
  def implementations(path: Path, at: LineColumn): Seq[SourceSpan] = host.read {
    val f = file(path)
    ProjectSearch.implementations(f, offset(document(f), at)).flatMap(nameSpan).distinct
  }

  /** The project's symbols whose names match `query`, best match first. */
  def workspaceSymbols(query: String): Seq[WorkspaceSymbolMatch] = host.read {
    ProjectSearch.symbols(host.project, query).flatMap { found =>
      nameSpan(found.element).map(WorkspaceSymbolMatch(found.element.name, workspaceSymbolKind(found.element), found.container, _))
    }
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
                Seq(OutlineSymbol(name, detail, workspaceSymbolKind(psi), span(whole.getTextRange), span(nameRange), children))
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
  def semanticTokens(path: Path): Seq[SemanticToken] = file(path) match {
    case scalaFile: ScalaFile =>
      host.read {
        val d = document(scalaFile)
        host.textAttributes(scalaFile).flatMap { (range, key) =>
          if (range.isEmpty || d.getLineNumber(range.getStartOffset) != d.getLineNumber(range.getEndOffset)) None
          else semanticToken(key).map { (kind, modifiers) =>
            SemanticToken(position(d, range.getStartOffset), range.getLength, kind, modifiers)
          }
        }
      }
    case _ => Seq.empty
  }

  private def problemSeverity(severity: HighlightSeverity): ProblemSeverity =
    if (severity.compareTo(HighlightSeverity.ERROR) >= 0) ProblemSeverity.Error
    else if (severity.compareTo(HighlightSeverity.WARNING) >= 0) ProblemSeverity.Warning
    else ProblemSeverity.Information

  private def workspaceSymbolKind(declaration: PsiElement): WorkspaceSymbolKind = declaration match {
    case _: ScEnumCase => WorkspaceSymbolKind.EnumMember
    case _: ScEnum => WorkspaceSymbolKind.Enum
    case _: ScTrait => WorkspaceSymbolKind.Interface
    case _: ScObject => WorkspaceSymbolKind.Module
    case _: ScTypeDefinition => WorkspaceSymbolKind.Class
    case function: ScFunction if function.isConstructor => WorkspaceSymbolKind.Constructor
    case function: ScFunction => if (function.containingClass == null) WorkspaceSymbolKind.Function else WorkspaceSymbolKind.Method
    case _: ScClassParameter => WorkspaceSymbolKind.Property
    case _: ScTypeAlias => WorkspaceSymbolKind.Class
    case _: ScExtension => WorkspaceSymbolKind.Namespace
    case named: ScNamedElement => named.nameContext match {
      case _: ScVariable => WorkspaceSymbolKind.Variable
      case _ => WorkspaceSymbolKind.Constant
    }
    case _ => WorkspaceSymbolKind.Object
  }

  private def semanticToken(key: TextAttributesKey): Option[(SemanticTokenKind, Set[SemanticTokenModifier])] = {
    import SemanticTokenKind.*
    import SemanticTokenModifier.*
    key match {
      case H.KEYWORD => Some(Keyword -> Set.empty)
      case H.PACKAGE => Some(Namespace -> Set.empty)
      case H.CLASS => Some(Class -> Set.empty)
      case H.ABSTRACT_CLASS => Some(Class -> Set(Abstract))
      case H.OBJECT => Some(Class -> Set(Readonly))
      case H.TRAIT => Some(Interface -> Set.empty)
      case H.ENUM => Some(Enum -> Set.empty)
      case H.ENUM_SINGLETON_CASE | H.ENUM_CLASS_CASE => Some(EnumMember -> Set.empty)
      case H.TYPEPARAM => Some(TypeParameter -> Set.empty)
      case H.TYPE_ALIAS => Some(Type -> Set.empty)
      case H.LOCAL_VALUES | H.LOCAL_LAZY | H.VALUES | H.LAZY | H.PATTERN | H.GENERATOR | H.GIVEN => Some(Variable -> Set(Readonly))
      case H.LOCAL_VARIABLES | H.VARIABLES => Some(Variable -> Set.empty)
      case H.CASE_CLASS_FIELD => Some(Property -> Set(Readonly))
      case H.CASE_CLASS_VAR_FIELD => Some(Property -> Set.empty)
      case H.PARAMETER | H.PARAMETER_OF_ANONIMOUS_FUNCTION | H.NAMED_ARGUMENT => Some(Parameter -> Set.empty)
      case H.METHOD_CALL | H.OBJECT_METHOD_CALL => Some(Method -> Set.empty)
      case H.LOCAL_METHOD_CALL => Some(Function -> Set.empty)
      case H.METHOD_DECLARATION => Some(Method -> Set(Declaration))
      case H.ANNOTATION => Some(Decorator -> Set.empty)
      case H.ANNOTATION_ATTRIBUTE => Some(Property -> Set.empty)
      case _ => None
    }
  }

}
