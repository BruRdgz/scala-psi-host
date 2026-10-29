package scalapsihost

import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.completion.impl.CompletionSorterImpl
import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils
import com.intellij.codeInsight.lookup.{Classifier, Lookup, LookupElement, LookupElementBuilder, LookupElementPresentation, LookupElementRenderer, WeighingContext}
import com.intellij.openapi.editor.Editor
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

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/**
 * BASIC completion as the IDE runs it: the plugin's contributors complete a copy of the file with a dummy identifier
 * at the caret, and their results are ordered by the plugin's weighers.
 *
 * As in the IDE, a suggestion's insert handler runs only once the user has chosen it ([[insert]]): the list inserts
 * the lookup string, and the handler (parentheses, an import, generated code) then runs on the text that has it,
 * through the [[ChangeEngine]].
 */
object Completion {
  // Bounds rendering and the response while leaving enough suggestions to narrow down as the user types on.
  val MaxSuggestions = 200

  /**
   * A suggestion as the IDE's lookup shows it. Choosing it replaces `prefix` (the typed `prefixText`) with `lookup`;
   * when `hasHandler`, its insert handler then runs ([[insert]]), and `identity` finds it again for that.
   */
  final case class Suggestion(
    label: String,
    tail: Option[String],
    typeText: Option[String],
    kind: CompletionItemKind,
    deprecated: Boolean,
    lookup: String,
    prefix: TextRange,
    prefixText: String,
    hasHandler: Boolean,
    identity: String,
  )

  /** The suggestions, best first; `incomplete` when some were left out, so the editor asks again as the user types. */
  final case class Suggestions(items: Seq[Suggestion], incomplete: Boolean)

  /** What a suggestion's insert handler changed: `result`'s edits, and where it left the caret in `result.text`. */
  final case class Inserted(result: ChangeEngine.Result, caret: Int)

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

  /** The suggestions at `offset`. A read: nothing is inserted until the user chooses. */
  def complete(file: ScalaFile, offset: Int, limit: Int = MaxSuggestions): Suggestions = HostLock.query {
    completionResults(file, file.getText, offset) { results =>
      // The lookup renders visible items lazily. One more successful rendering determines whether the list is incomplete.
      val suggestions = sort(results).iterator.flatMap(result => rendered(result, offset))
      val items = suggestions.take(limit).toVector
      Suggestions(items, incomplete = suggestions.hasNext)
    }
  }

  /**
   * Runs the insert handler of the suggestion `identity`, chosen where `prefixText` was typed at `start` and whose
   * lookup string `file` now has there, as the IDE does once the user chooses it. Empty when the file no longer has
   * the lookup string there, or the suggestion is gone.
   */
  def insert(file: ScalaFile, start: Int, prefixText: String, lookup: String, identity: String): Option[Inserted] =
    HostLock.query {
      val end = start + lookup.length
      val text = file.getText
      if (!text.startsWith(lookup, start)) None
      else {
        // The suggestion is found again where it was made: in the text with the typed prefix, not the lookup string.
        val typed = text.substring(0, start) + prefixText + text.substring(end)
        val offset = start + prefixText.length
        completionResults(file, typed, offset) { results =>
          val chosen = results.find(result => result.getLookupElement.getLookupString == lookup &&
            rendered(result, offset).exists(_.identity == identity))
          chosen.map(handle(file, _, results, new TextRange(start, end), Lookup.NORMAL_SELECT_CHAR))
        }
      }
    }

  /**
   * Chooses, at `offset`, the best suggestion that `lookup` names, as the plugin's completion tests do: its lookup
   * string replaces the prefix, then its insert handler runs as on `completionChar`.
   */
  private[scalapsihost] def choose(file: ScalaFile, offset: Int, lookup: String, completionChar: Char): Inserted =
    HostLock.query {
      completionResults(file, file.getText, offset) { results =>
        val chosen = sort(results).find(_.getLookupElement.getAllLookupStrings.contains(lookup))
          .getOrElse(throw new IllegalArgumentException(s"no suggestion for $lookup"))
        handle(file, chosen, results, new TextRange(offset - chosen.getPrefixMatcher.getPrefix.length, offset), completionChar)
      }
    }

  /** Replaces `prefix` with the chosen lookup string and runs its insert handler, on a copy of the file. */
  private def handle(file: ScalaFile, chosen: CompletionResult, results: Vector[CompletionResult], prefix: TextRange,
                     completionChar: Char): Inserted = {
    val element = chosen.getLookupElement
    val (changed, editor) = ChangeEngine.run(file) { copy =>
      val document = copy.getViewProvider.getDocument
      document.replaceString(prefix.getStartOffset, prefix.getEndOffset, element.getLookupString)
      val editor = IntentionPreviewUtils.getPreviewEditor
      editor.getCaretModel.moveToOffset(prefix.getStartOffset + element.getLookupString.length)
      runHandler(copy, editor, element, results.map(_.getLookupElement).toArray, prefix.getStartOffset, completionChar)
      editor
    }
    Inserted(changed, editor.getCaretModel.getOffset)
  }

  /**
   * Runs `element`'s insert handler on `copy`, which has its lookup string at `start`, with the context the IDE's
   * lookup gives it: the offsets of the inserted string and of the identifier it ends in, the suggestions, and the
   * character that chose it.
   */
  private def runHandler(copy: ScalaFile, editor: Editor, element: LookupElement, elements: Array[LookupElement],
                         start: Int, completionChar: Char): Unit = {
    val document = copy.getViewProvider.getDocument
    val tail = editor.getCaretModel.getOffset
    val identifierEnd = CompletionInitializationContext.calcDefaultIdentifierEnd(editor, tail)
    val offsets = new OffsetMap(document)
    offsets.addOffset(CompletionInitializationContext.START_OFFSET, start)
    offsets.addOffset(CompletionInitializationContext.SELECTION_END_OFFSET, tail)
    offsets.addOffset(CompletionInitializationContext.IDENTIFIER_END_OFFSET, identifierEnd)
    try {
      // Replacing (Tab) also removes the rest of the identifier the caret was in.
      if (completionChar == Lookup.REPLACE_SELECT_CHAR && identifierEnd > tail) document.deleteString(tail, identifierEnd)
      val context = new InsertionContext(offsets, completionChar, elements, copy, editor, false)
      context.commitDocument()
      element.handleInsert(context)
      // What a handler leaves for later is UI (a parameter info popup, a dialog): its edits are made, the UI is not.
      if (context.getLaterRunnable != null) CallLog.hit("Completion.laterRunnableSkipped")
    } finally offsets.dispose()
  }

  private def rendered(result: CompletionResult, offset: Int): Option[Suggestion] =
    try Some(suggestion(result, offset))
    catch {
      case e: ProcessCanceledException => throw e
      case NonFatal(e) =>
        // One suggestion the plugin fails to render must not take the list with it; the error is reported.
        HostLog.error(s"rendering the suggestion ${result.getLookupElement.getLookupString} failed", e)
        CallLog.hit("Completion.renderingFailed")
        None
    }

  private def suggestion(result: CompletionResult, offset: Int): Suggestion = {
    val element = result.getLookupElement
    val presentation = new LookupElementPresentation
    element.renderElement(presentation)
    // The cheap rendering leaves out Scala's types and signatures: they come from the expensive renderer.
    Option(element.getExpensiveRenderer).foreach { renderer =>
      // The API types the renderer existentially; its contract is to render this very element.
      renderer.asInstanceOf[LookupElementRenderer[LookupElement]].renderElement(element, presentation)
    }
    def text(value: String) = Option(value).map(_.trim).filter(_.nonEmpty)
    val label = text(presentation.getItemText).getOrElse(element.getLookupString)
    val tail = text(presentation.getTailText)
    val typeText = text(presentation.getTypeText)
    Suggestion(
      label = label,
      tail = tail,
      typeText = typeText,
      kind = kind(element),
      deprecated = presentation.isStrikeout,
      lookup = element.getLookupString,
      prefix = new TextRange(offset - result.getPrefixMatcher.getPrefix.length, offset),
      prefixText = result.getPrefixMatcher.getPrefix,
      hasHandler = hasHandler(element),
      // What the lookup shows tells overloads apart; the element's class, suggestions of different contributors.
      identity = Seq(element.getClass.getName, label, tail.getOrElse(""), typeText.getOrElse("")).mkString("\u0000"),
    )
  }

  /** Whether choosing `element` does more than insert its lookup string (ScalaInsertHandler, an import, a keyword's space...). */
  private def hasHandler(element: LookupElement): Boolean = ScalaLookupItem.delegate(element) match {
    case item: ScalaLookupItem => !insertsLookupOnly(item)
    case builder: LookupElementBuilder => builder.getInsertHandler != null
    case _ => true
  }

  /** Whether ScalaInsertHandler would leave the lookup string as it is: no parentheses, quotes, backticks or import. */
  private def insertsLookupOnly(item: ScalaLookupItem): Boolean = {
    val changesText = item.getInsertHandler != null && !item.getInsertHandler.isInstanceOf[ScalaInsertHandler] ||
      item.isClassName || item.prefixCompletion || item.shouldImport || item.elementToImport.nonEmpty ||
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
   * The completion results at `offset` of `text`, the text of `file` or one it had, while the copy they were
   * computed on (and what it holds) is alive: the contributors run before completion (choosing the dummy
   * identifier), then complete a copy where it replaces the prefix, as the IDE's CompletionInitializationUtil does.
   */
  private def completionResults[T](file: ScalaFile, text: String, offset: Int)(use: Vector[CompletionResult] => T): T =
    Using.Manager { resources =>
      require(offset >= 0 && offset <= text.length, s"completion offset outside the text: $offset")
      ProgressManager.checkCanceled()
      // Other text than the file's, or a file of the plugin's PSI factory (which has no document), is completed on a copy.
      val asIs = file.isPhysical && text == file.getText
      val document = if (asIs) file.getViewProvider.getDocument else new DocumentImpl(text)
      val editor = resources(new RequestEditor(document, file.getProject, offset))
      val initializationFile = if (asIs) file else resources(new TemporaryCopy(file, text)).file
      val init = new CompletionInitializationContext(editor.editor, editor.editor.getCaretModel.getCurrentCaret,
        file.getLanguage, initializationFile, CompletionType.BASIC, 1)
      releasing(resources)(init.getOffsetMap.dispose())
      CompletionContributor.forLanguageHonorDumbness(file.getLanguage, file.getProject, editor.editor).asScala.foreach { contributor =>
        ProgressManager.checkCanceled()
        contributor.beforeCompletion(init)
      }
      val copied = dummyIdentifierCopy(file, text, init, offset, resources)
      val translator = new OffsetTranslator(document, initializationFile, copied.getOffsets.getDocument, init.getStartOffset,
        init.getSelectionEndOffset, init.getDummyIdentifier)
      releasing(resources)(Disposer.dispose(translator))
      val parameters = CompletionInitializationUtil.createCompletionParameters(init, () => false, copied)
      val results = Vector.newBuilder[CompletionResult]
      CompletionService.getCompletionService.performCompletion(parameters, result => results += result)
      use(results.result())
    }.get

  /** The copy with the dummy identifier in place of the prefix, and the initialization offsets moved past it. */
  private def dummyIdentifierCopy(file: ScalaFile, text: String, init: CompletionInitializationContext, offset: Int,
                                  resources: Using.Manager): OffsetsInFile = {
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
      // Sorters are compared by value: contributors make equal ones for each result.
      val group = results.filter(sorterOf(_) == sorter)
      // Equal weights are ordered by source position, never by hash iteration, so the order is the same every time.
      classified(sorter, group.map(_.getLookupElement).sortBy(sourceOrder), context).map(byElement)
    }
  }

  /**
   * `elements` in the order of `sorter`'s weighers. One a weigher fails on is logged and left out, and the others are
   * weighed again by a new classifier (the failed one may have weighed it in part): the rest still make a list.
   */
  @tailrec
  private def classified(sorter: CompletionSorterImpl, elements: Vector[LookupElement], context: ProcessingContext): Vector[LookupElement] = {
    val classifier = sorter.buildClassifier(Classifier.empty[LookupElement]())
    val failed = elements.find { element =>
      ProgressManager.checkCanceled()
      try {
        classifier.addElement(element, context)
        false
      } catch {
        case e: ProcessCanceledException => throw e
        case NonFatal(e) =>
          HostLog.error(s"weighing the suggestion ${describe(element)} failed", e)
          CallLog.hit("Completion.weighingFailed")
          true
      }
    }
    failed match {
      case Some(element) => classified(sorter, elements.filterNot(_ eq element), context)
      case None => classifier.classify(elements.asJava, context).asScala.toVector
    }
  }

  /** What a failure report needs to find the element: its kind, and where its PSI is (or is not). */
  private def describe(element: LookupElement): String = {
    val psi = Option(element.getPsiElement)
    val file = psi.flatMap(e => Option(e.getContainingFile)).fold("no file")(f => s"in ${f.getName} (${f.getClass.getSimpleName})")
    s"${element.getLookupString} (${element.getClass.getSimpleName}; ${psi.fold("no PSI")(e => s"${e.getClass.getSimpleName}, valid ${e.isValid}, $file")})"
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
