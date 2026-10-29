package scalapsihost

import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.completion.impl.CompletionSorterImpl
import com.intellij.codeInsight.lookup.{Classifier, Lookup, LookupElement, LookupElementBuilder, LookupElementPresentation, LookupElementRenderer, WeighingContext}
import com.intellij.openapi.editor.impl.DocumentImpl
import com.intellij.openapi.progress.{ProcessCanceledException, ProgressManager}
import com.intellij.openapi.util.{Disposer, TextRange}
import com.intellij.psi.*
import com.intellij.psi.impl.PsiManagerEx
import com.intellij.psi.impl.source.PsiFileImpl
import com.intellij.util.{Consumer, ProcessingContext}
import org.eclipse.lsp4j.CompletionItemKind
import org.jetbrains.plugins.scala.lang.completion.handlers.ScalaInsertHandler
import org.jetbrains.plugins.scala.lang.completion.lookups.ScalaKeywordLookupItem.KeywordInsertHandler
import org.jetbrains.plugins.scala.lang.completion.lookups.ScalaLookupItem
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScEnumCase, ScFunction, ScTypeAlias, ScValueOrVariable}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter, ScTypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScEnum, ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.ScSyntheticFunction
import org.jetbrains.plugins.scala.project.ScalaFeatures
import scalapsihost.ChangeEngine.Edit

import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/**
 * BASIC completion as the IDE runs it: the plugin's contributors complete a copy of the file with a dummy identifier
 * at the caret, their results are ordered by the plugin's weighers, and choosing one runs its insert handler, here on
 * another copy through the [[ChangeEngine]].
 */
object Completion {
  // Bounds rendering and the response while leaving enough suggestions to narrow down as the user types on.
  val MaxSuggestions = 200
  // Suggestions whose insert handler must run to know their edits; each one is a change on a copy of the file.
  val MaxPreviews = 8

  /** A suggestion as the IDE's lookup shows it, and what choosing it does. */
  final case class Suggestion(
    label: String,
    tail: Option[String],
    typeText: Option[String],
    kind: CompletionItemKind,
    deprecated: Boolean,
    filterText: String,
    insertion: Insertion,
  )

  /**
   * What choosing a suggestion does: `edit` replaces the completed prefix (and whatever the insert handler changed
   * around it, on the same line), the caret ends `caret` characters into its text, and `additional` edits change the
   * file elsewhere (imports).
   */
  final case class Insertion(edit: Edit, caret: Int, additional: Seq[Edit])

  /** The suggestions, best first; `incomplete` when some were left out, so the editor asks again as the user types. */
  final case class Suggestions(items: Seq[Suggestion], incomplete: Boolean)

  /** An insert handler that cannot run here (it needs the IDE's UI or templates) or whose edits LSP cannot express. */
  private final class UnsupportedInsertion(reason: String) extends UnsupportedOperationException(reason)

  /** How a suggestion's insertion is known. */
  private enum Handling {
    /** Its lookup string replaces the prefix: its handler, if any, changes nothing. */
    case Plain
    /** Its handler must run on a copy of the file. */
    case Preview
    case Unsupported(reason: String)
  }

  /** The platform service keeps the current completion in a field; host requests complete on several threads. */
  private[scalapsihost] final class Service extends BaseCompletionService {
    private val current = new ThreadLocal[CompletionProcess]
    override def getCurrentCompletion: CompletionProcess = current.get()
    override def performCompletion(parameters: CompletionParameters, consumer: Consumer[? >: CompletionResult]): Unit = {
      current.set(parameters.getProcess)
      try super.performCompletion(parameters, consumer)
      finally current.remove()
    }
  }

  /** The suggestions at `offset`; at most `previews` of them run their insert handler. */
  def complete(file: ScalaFile, offset: Int, limit: Int = MaxSuggestions, previews: Int = MaxPreviews): Suggestions =
    HostLock.modification {
      completionResults(file, offset) { results =>
        val ranked = sort(results)
        val elements = ranked.map(_.getLookupElement).toArray
        val handled = ranked.map(result => result -> handling(result.getLookupElement))
        val previewed = handled.collect { case (result, Handling.Preview) => result }.take(previews).toSet
        val suggestions = handled.flatMap {
          case (result, Handling.Plain) => rendered(result, plainInsertion(result, offset))
          case (result, Handling.Preview) if previewed(result) => preview(file, offset, result, elements).flatMap(rendered(result, _))
          case (_, Handling.Preview) => skipped("previewBudget")
          case (_, Handling.Unsupported(reason)) => skipped(reason)
        }
        Suggestions(suggestions.take(limit), incomplete = suggestions.size > limit || suggestions.size < ranked.size)
      }
    }

  /** The insertion of the suggestion whose lookup strings include `lookup`, as the IDE inserts it on `completionChar`. */
  def insertion(file: ScalaFile, offset: Int, lookup: String, completionChar: Char = Lookup.NORMAL_SELECT_CHAR): Insertion =
    HostLock.modification {
      completionResults(file, offset) { results =>
        val found = results.filter(_.getLookupElement.getAllLookupStrings.contains(lookup))
        require(found.nonEmpty, s"no suggestion for $lookup")
        val elements = sort(results).map(_.getLookupElement).toArray
        val insertions = found.map { result =>
          handling(result.getLookupElement) match {
            case Handling.Unsupported(reason) => throw new UnsupportedInsertion(reason)
            case _ => insert(file, offset, result, elements, completionChar)
          }
        }.distinct
        require(insertions.size == 1, s"suggestions for $lookup insert differently")
        insertions.head
      }
    }

  private def skipped[T](reason: String): Option[T] = {
    CallLog.hit(s"Completion.skipped.$reason")
    None
  }

  /** A suggestion the plugin fails to render is logged and left out: the others still make a list. */
  private def rendered(result: CompletionResult, insertion: Insertion): Option[Suggestion] =
    try Some(suggestion(result, insertion))
    catch {
      case e: ProcessCanceledException => throw e
      case NonFatal(e) =>
        HostLog.error(s"rendering the suggestion ${result.getLookupElement.getLookupString} failed", e)
        skipped("renderingFailed")
    }

  private def suggestion(result: CompletionResult, insertion: Insertion): Suggestion = {
    val element = result.getLookupElement
    val presentation = new LookupElementPresentation
    element.renderElement(presentation)
    // The cheap rendering leaves out Scala's types and signatures: they come from the expensive renderer.
    Option(element.getExpensiveRenderer).foreach { renderer =>
      // The API types the renderer existentially; its contract is to render this very element.
      renderer.asInstanceOf[LookupElementRenderer[LookupElement]].renderElement(element, presentation)
    }
    def text(value: String) = Option(value).map(_.trim).filter(_.nonEmpty)
    Suggestion(
      label = text(presentation.getItemText).getOrElse(element.getLookupString),
      tail = text(presentation.getTailText),
      typeText = text(presentation.getTypeText),
      kind = kind(element),
      deprecated = presentation.isStrikeout,
      filterText = element.getLookupString,
      insertion = insertion,
    )
  }

  private def plainInsertion(result: CompletionResult, offset: Int): Insertion = {
    val lookup = result.getLookupElement.getLookupString
    Insertion(Edit(prefixRange(result, offset), lookup), lookup.length, Seq.empty)
  }

  private def prefixRange(result: CompletionResult, offset: Int): TextRange =
    new TextRange(offset - result.getPrefixMatcher.getPrefix.length, offset)

  private def preview(file: ScalaFile, offset: Int, result: CompletionResult, elements: Array[LookupElement]): Option[Insertion] =
    try Some(insert(file, offset, result, elements, Lookup.NORMAL_SELECT_CHAR))
    catch {
      case e: UnsupportedInsertion => skipped(e.getMessage)
      case e: UnsupportedOperationException =>
        HostLog.warn(s"the insert handler of ${result.getLookupElement.getLookupString} needs unsupported platform code", e)
        skipped("unsupportedPlatform")
    }

  private def handling(element: LookupElement): Handling = ScalaLookupItem.delegate(element) match {
    case item: ScalaLookupItem => Option(item.getInsertHandler) match {
      case None | Some(_: ScalaInsertHandler) => if (isPlain(item)) Handling.Plain else Handling.Preview
      case Some(handler) => Handling.Unsupported(s"handler.${handler.getClass.getName}")
    }
    case builder: LookupElementBuilder => Option(builder.getInsertHandler) match {
      case None => Handling.Plain
      case Some(handler: KeywordInsertHandler) =>
        if (KeywordsWithoutSpace(handler.keyword)) Handling.Plain else Handling.Preview
      case Some(handler) if handler.getClass.getName == ConstructorInsertHandler =>
        if (isAbstract(element.getPsiElement)) Handling.Unsupported("implementationDialog") else Handling.Preview
      case Some(handler) => Handling.Unsupported(s"handler.${handler.getClass.getName}")
    }
    case other => Handling.Unsupported(s"lookup.${other.getClass.getName}")
  }

  // Keywords the keyword handler inserts as they are, without a following space.
  private val KeywordsWithoutSpace = Set("this", "false", "true", "null", "super")
  // Private in the plugin: known by name.
  private val ConstructorInsertHandler = "org.jetbrains.plugins.scala.lang.completion.handlers.ScalaConstructorInsertHandler"

  // An abstract class or trait asks the IDE's dialog for the members to implement.
  private def isAbstract(element: PsiElement): Boolean = element match {
    case cls: PsiClass => cls.isInterface || cls.hasModifierProperty(PsiModifier.ABSTRACT)
    case _ => false
  }

  /** Whether ScalaInsertHandler would leave the lookup string as it is: no parentheses, quotes, backticks or import. */
  private def isPlain(item: ScalaLookupItem): Boolean = {
    val changesText = item.isClassName || item.prefixCompletion || item.shouldImport || item.elementToImport.nonEmpty ||
      item.someSmartCompletion || item.isInSimpleString || item.isInInterpolatedString ||
      item.isNamedParameterOrAssignment || item.isInStableElementPattern || item.getLookupString.contains("`")
    !changesText && (item.getPsiElement match {
      case function: ScFunction =>
        // classOf gets its type argument brackets.
        function.name != "classOf" && ScalaInsertHandler.getItemParametersAndAccessorStatus(function)._1 < 0
      case function: ScSyntheticFunction => function.paramClauses.isEmpty
      case _: PsiMethod | _: ScTypeDefinition => false
      case _ => true
    })
  }

  /**
   * Inserts `selected` as the IDE does: its lookup string over the prefix, then its insert handler, with the
   * suggestions around and an editor whose caret follows the edits. The handler runs on a copy of the file with
   * the original text, never on the dummy-identifier copy the suggestions came from.
   */
  private def insert(file: ScalaFile, offset: Int, selected: CompletionResult, elements: Array[LookupElement],
                     completionChar: Char): Insertion = {
    val prefix = prefixRange(selected, offset)
    val (result, editor) = ChangeEngine.run(file) { copy =>
      val document = copy.getViewProvider.getDocument
      val lookup = selected.getLookupElement.getLookupString
      document.replaceString(prefix.getStartOffset, prefix.getEndOffset, lookup)
      val editor = new CompletionEditor(document, copy.getProject, prefix.getStartOffset + lookup.length)
      runHandler(copy, editor, selected.getLookupElement, elements, prefix.getStartOffset, completionChar)
      editor
    }
    try insertionEdits(file.getText, result, prefix, editor.editor.getCaretModel.getOffset)
    finally editor.close()
  }

  private def runHandler(copy: ScalaFile, editor: CompletionEditor, element: LookupElement, elements: Array[LookupElement],
                         start: Int, completionChar: Char): Unit = {
    val document = copy.getViewProvider.getDocument
    val tail = editor.editor.getCaretModel.getOffset
    val identifierEnd = CompletionInitializationContext.calcDefaultIdentifierEnd(editor.editor, tail)
    val offsets = new OffsetMap(document)
    offsets.addOffset(CompletionInitializationContext.START_OFFSET, start)
    offsets.addOffset(CompletionInitializationContext.SELECTION_END_OFFSET, tail)
    offsets.addOffset(CompletionInitializationContext.IDENTIFIER_END_OFFSET, identifierEnd)
    try {
      // Replacing (Tab) also removes the rest of the identifier the caret was in.
      if (completionChar == Lookup.REPLACE_SELECT_CHAR && identifierEnd > tail) document.deleteString(tail, identifierEnd)
      val context = new InsertionContext(offsets, completionChar, elements, copy, editor.editor, false)
      context.commitDocument()
      element.handleInsert(context)
      // Work the handler leaves for later is UI (popups, dialogs) the host cannot show.
      if (context.getLaterRunnable != null) throw new UnsupportedInsertion("laterRunnable")
    } finally offsets.dispose()
  }

  /**
   * LSP wants a completion item's main edit on one line, around the completed prefix: the insertion's edits that
   * meet the prefix or the caret become that edit; the others (imports) are additional edits.
   */
  private def insertionEdits(original: String, result: ChangeEngine.Result, prefix: TextRange, caret: Int): Insertion = {
    val main = covering(result.edits, prefix.union(originalRange(result.edits, caret)))
    if (!lineOf(original, prefix.getStartOffset).contains(main)) throw new UnsupportedInsertion("editOutsideLine")
    val (local, additional) = result.edits.partition(edit => meet(edit.range, main))
    val start = main.getStartOffset + additional.filter(_.range.getStartOffset < main.getStartOffset).map(lengthChange).sum
    val end = start + main.getLength + local.map(lengthChange).sum
    if (caret < start || caret > end) throw new UnsupportedInsertion("caretOutsideEdit")
    Insertion(Edit(main, result.text.substring(start, end)), caret - start, additional)
  }

  private def lengthChange(edit: Edit): Int = edit.text.length - edit.range.getLength

  private def meet(a: TextRange, b: TextRange): Boolean = a.getEndOffset >= b.getStartOffset && a.getStartOffset <= b.getEndOffset

  /** `range` grown by the edits that meet it, and by those that meet these: the edits are sorted and disjoint. */
  private def covering(edits: Seq[Edit], range: TextRange): TextRange =
    edits.foldLeft(range)((covered, edit) => if (meet(edit.range, covered)) covered.union(edit.range) else covered)

  /** Where `changed`, an offset in the changed text, comes from in the original: an edit's range when it is in its new text. */
  private def originalRange(edits: Seq[Edit], changed: Int): TextRange = {
    // How far each edit's start has moved by the edits before it.
    val shifts = edits.scanLeft(0)((shift, edit) => shift + lengthChange(edit))
    edits.zip(shifts).collectFirst {
      case (edit, shift) if changed < edit.range.getStartOffset + shift => TextRange.from(changed - shift, 0)
      case (edit, shift) if changed <= edit.range.getStartOffset + shift + edit.text.length => edit.range
    }.getOrElse(TextRange.from(changed - shifts.last, 0))
  }

  private def lineOf(text: String, offset: Int): TextRange = {
    val end = text.indexOf('\n', offset)
    new TextRange(text.lastIndexOf('\n', offset - 1) + 1, if (end < 0) text.length else end)
  }

  /**
   * The completion results at `offset`, while the copy they were computed on (and what it holds) is alive: the
   * contributors run before completion (choosing the dummy identifier), then complete a copy where it replaces the
   * prefix, as the IDE's CompletionInitializationUtil does.
   */
  private def completionResults[T](file: ScalaFile, offset: Int)(use: Vector[CompletionResult] => T): T = Using.Manager { resources =>
    require(offset >= 0 && offset <= file.getTextLength, s"completion offset outside the file: $offset")
    ProgressManager.checkCanceled()
    // A file the plugin's PSI factory made has no document of its own.
    val document = if (file.isPhysical) file.getViewProvider.getDocument else new DocumentImpl(file.getText)
    val editor = resources(new CompletionEditor(document, file.getProject, offset))
    val initializationFile = if (file.isPhysical) file else resources(new TemporaryCopy(file, file.getText)).file
    val init = new CompletionInitializationContext(editor.editor, editor.editor.getCaretModel.getCurrentCaret,
      file.getLanguage, initializationFile, CompletionType.BASIC, 1)
    releasing(resources)(init.getOffsetMap.dispose())
    CompletionContributor.forLanguageHonorDumbness(file.getLanguage, file.getProject, editor.editor).asScala.foreach { contributor =>
      ProgressManager.checkCanceled()
      contributor.beforeCompletion(init)
    }
    val copied = dummyIdentifierCopy(file, init, offset, resources)
    val translator = new OffsetTranslator(document, file, copied.getOffsets.getDocument, init.getStartOffset,
      init.getSelectionEndOffset, init.getDummyIdentifier)
    releasing(resources)(Disposer.dispose(translator))
    val parameters = CompletionInitializationUtil.createCompletionParameters(init, () => false, copied)
    val results = Vector.newBuilder[CompletionResult]
    CompletionService.getCompletionService.performCompletion(parameters, result => results += result)
    use(results.result())
  }.get

  /** The copy with the dummy identifier in place of the prefix, and the initialization offsets moved past it. */
  private def dummyIdentifierCopy(file: ScalaFile, init: CompletionInitializationContext, offset: Int,
                                  resources: Using.Manager): OffsetsInFile = {
    val text = file.getText
    val dummy = init.getDummyIdentifier
    val copy = resources(new TemporaryCopy(file, text.substring(0, init.getStartOffset) + dummy + text.substring(init.getSelectionEndOffset))).file
    val offsets = new OffsetMap(copy.getViewProvider.getDocument)
    releasing(resources)(offsets.dispose())
    for (key <- init.getOffsetMap.getAllOffsets.asScala) {
      val at = init.getOffsetMap.getOffset(key)
      offsets.addOffset(key, if (at > offset || at == offset && key.isMovableToRight) at + dummy.length else at)
    }
    new OffsetsInFile(copy, offsets)
  }

  private def releasing(resources: Using.Manager)(release: => Unit): Unit = {
    val closeable: AutoCloseable = () => release
    resources(closeable)
  }

  /**
   * A non-physical copy of `file` with `text`, for completion's own reading. Unlike PsiFile.copy (which the change
   * engine uses), it never takes a write: completion only reads.
   */
  private final class TemporaryCopy(original: ScalaFile, text: String) extends AutoCloseable {
    val file: PsiFile = PsiFileFactory.getInstance(original.getProject)
      .createFileFromText(original.getName, original.getLanguage, text, false, false)
    file match {
      case source: PsiFileImpl => source.setOriginalFile(original)
      case other => throw new IllegalStateException(s"completion copy is not a source file: ${other.getClass.getName}")
    }
    // Module scopes come from the original file; Scala features are plain user data, set before the tree is parsed.
    ScalaFeatures.setAttachedScalaFeatures(file, ScalaFeatures.forPsiOrDefault(original))

    /** The file manager keeps a copy's view provider until told otherwise (see commit e098341). */
    override def close(): Unit =
      PsiManagerEx.getInstanceEx(original.getProject).getFileManager.setViewProvider(file.getViewProvider.getVirtualFile, null)
  }

  /** Each result keeps the sorter of the contributor that made it: results are ordered within their sorter's group. */
  private def sort(results: Vector[CompletionResult]): Vector[CompletionResult] = {
    val byElement = results.map(result => result.getLookupElement -> result).toMap
    val context = weighingContext(byElement)
    results.map(sorterOf).distinct.flatMap { sorter =>
      val group = results.filter(sorterOf(_) eq sorter)
      val classifier = sorter.buildClassifier(Classifier.empty[LookupElement]())
      // Equal weights are ordered by source position, never by hash iteration, so the order is the same every time.
      val elements = group.map(_.getLookupElement).sortBy(sourceOrder)
      elements.foreach { element =>
        ProgressManager.checkCanceled()
        classifier.addElement(element, context)
      }
      classifier.classify(elements.asJava, context).asScala.map(byElement).toVector
    }
  }

  private def sorterOf(result: CompletionResult): CompletionSorterImpl = result.getSorter match {
    case sorter: CompletionSorterImpl => sorter
    case other => throw new IllegalStateException(s"unsupported completion sorter: ${other.getClass.getName}")
  }

  // What the IDE's lookup arranger gives the weighers: each item's own prefix matcher, and no typing since it opened.
  private def weighingContext(byElement: Map[LookupElement, CompletionResult]): ProcessingContext = {
    val context = new ProcessingContext
    context.put(CompletionLookupArranger.PREFIX_CHANGES, Integer.valueOf(0))
    context.put(CompletionLookupArranger.WEIGHING_CONTEXT, new WeighingContext {
      override def itemMatcher(item: LookupElement): PrefixMatcher = byElement(item).getPrefixMatcher
      override def itemPattern(item: LookupElement): String = itemMatcher(item).getPrefix
    })
    context
  }

  private def sourceOrder(element: LookupElement): (String, String, Int, String) = {
    val psi = Option(element.getPsiElement)
    val path = psi.flatMap(e => Option(e.getContainingFile)).map(_.getViewProvider.getVirtualFile.getPath).getOrElse("")
    // Synthetic functions (the standard library's operators) have no source: their signature tells overloads apart.
    val signature = psi match {
      case Some(function: ScSyntheticFunction) =>
        function.paramClauses.map(_.map(_.paramType.canonicalText).mkString("(", ",", ")")).mkString + function.retType.canonicalText
      case _ => ""
    }
    (element.getLookupString, path, psi.fold(-1)(_.getTextOffset), signature)
  }

  private def kind(element: LookupElement): CompletionItemKind = element.getPsiElement match {
    case _: ScEnumCase => CompletionItemKind.EnumMember
    case _: ScEnum => CompletionItemKind.Enum
    case _: ScTrait => CompletionItemKind.Interface
    case _: ScObject => CompletionItemKind.Module
    case _: ScTypeParam | _: PsiTypeParameter => CompletionItemKind.TypeParameter
    case _: ScTypeDefinition | _: ScTypeAlias | _: PsiClass => CompletionItemKind.Class
    case function: ScFunction if function.isConstructor => CompletionItemKind.Constructor
    case _: ScFunction | _: PsiMethod | _: ScSyntheticFunction => CompletionItemKind.Method
    case _: ScClassParameter => CompletionItemKind.Field
    case _: ScParameter | _: PsiParameter => CompletionItemKind.Variable
    case _: PsiField => CompletionItemKind.Field
    case _: PsiPackage => CompletionItemKind.Module
    case named: ScNamedElement =>
      if (Option(element.as(classOf[ScalaLookupItem])).exists(_.isLocalVariable)) CompletionItemKind.Variable
      else named.nameContext match {
        case _: ScValueOrVariable => CompletionItemKind.Field
        case _ => CompletionItemKind.Value
      }
    case _ if isKeyword(element) => CompletionItemKind.Keyword
    case _ => CompletionItemKind.Value
  }

  private def isKeyword(element: LookupElement): Boolean = element match {
    case builder: LookupElementBuilder => builder.getInsertHandler.isInstanceOf[KeywordInsertHandler]
    case _ => false
  }
}
