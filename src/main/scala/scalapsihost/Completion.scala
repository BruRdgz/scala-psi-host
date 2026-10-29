package scalapsihost

import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.completion.impl.CompletionSorterImpl
import com.intellij.codeInsight.lookup.{Classifier, LookupElement, LookupElementBuilder, LookupElementPresentation, LookupElementRenderer, WeighingContext}
import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.Disposer
import com.intellij.psi.*
import com.intellij.util.ProcessingContext
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.{Either as LspEither}
import org.jetbrains.plugins.scala.lang.completion.lookups.ScalaLookupItem
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScEnumCase, ScFunction, ScTypeAlias, ScValueOrVariable}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter, ScTypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScEnum, ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.project.ScalaFeatures

import scala.jdk.CollectionConverters.*

/** BASIC completion and insertion replay share the plugin's lookup elements without retaining PSI between requests. */
object Completion {
  // Bounds rendering and the JSON response while leaving enough candidates for interactive prefix refinement.
  val MaxItems = 200

  // A list previews at most eight primary-changing handlers; replaying all 200 would serialize interactive queries.
  val MaxEagerHandlers = 8
  final case class Options(snippets: Boolean = false, resolve: Boolean = false, uri: String = "", version: Int = 0)
  private[scalapsihost] final case class Recipe(uri: String, version: Int, stamp: Long, offset: Int, lookup: String, identity: String, snippets: Boolean)
  private final case class Insertion(primary: TextEdit, additional: Seq[TextEdit])
  private final class UnsupportedInsertion(reason: String) extends UnsupportedOperationException(reason)

  /** The base service's current process is shared in the IDE; host queries can run on several read threads. */
  private[scalapsihost] final class Service extends BaseCompletionService {
    private val current = new ThreadLocal[CompletionProcess]
    override def getCurrentCompletion: CompletionProcess = current.get()
    override def performCompletion(parameters: CompletionParameters, consumer: com.intellij.util.Consumer[? >: CompletionResult]): Unit = {
      current.set(parameters.getProcess)
      try super.performCompletion(parameters, consumer)
      finally current.remove()
    }
  }

  private[scalapsihost] def complete(file: ScalaFile, offset: Int, limit: Int = MaxItems,
                                    options: Options = Options(), keywordOnly: Boolean = false,
                                    maxPreviews: Int = MaxEagerHandlers): CompletionList = HostLock.modification {
    withResults(file, offset) { (document, results) =>
      val sorted = sort(if (keywordOnly) results.filter(r => kind(r.getLookupElement) == CompletionItemKind.Keyword) else results)
      val elements = sorted.map(_.getLookupElement).toArray
      var previews = 0
      var omitted = false
      val items = sorted.iterator.zipWithIndex.flatMap { (result, rank) =>
        ProgressManager.checkCanceled()
        val element = result.getLookupElement
        val plain = plainInsertion(element)
        unsupportedReason(element) match {
          case Some(reason) => CallLog.hit("Completion.filtered." + reason); omitted = true; None
          case None if !plain && previews >= maxPreviews =>
            CallLog.hit("Completion.filtered.eagerBudget")
            omitted = true
            None
          case None =>
            val item = toItem(result, document, offset, rank)
            if (plain) {
              if (options.snippets) {
                val insertion = item.getTextEdit.getLeft.getNewText
                item.getTextEdit.getLeft.setNewText(snippet(insertion, insertion.length))
                item.setInsertTextFormat(InsertTextFormat.Snippet)
              }
              Some(item)
            } else {
              previews += 1
              try {
                val insertion = insert(file, offset, result, elements, options.snippets)
                item.setTextEdit(LspEither.forLeft(insertion.primary))
                item.setInsertTextFormat(if (options.snippets) InsertTextFormat.Snippet else InsertTextFormat.PlainText)
                val selection = if (!options.resolve || insertion.additional.isEmpty) None
                else {
                  val id = identity(result)
                  val unique = results.count(r => r.getLookupElement.getLookupString == element.getLookupString && identity(r) == id) == 1
                  Option.when(unique)(Recipe(options.uri, options.version, file.getViewProvider.getDocument.getModificationStamp,
                    offset, element.getLookupString, id, options.snippets))
                }
                selection match {
                  case Some(recipe) => item.setData(new com.google.gson.Gson().toJsonTree(recipe))
                  case None => item.setAdditionalTextEdits(insertion.additional.asJava)
                }
                Some(item)
              } catch {
                case e: UnsupportedInsertion =>
                  CallLog.hit("Completion.filtered." + e.getMessage)
                  omitted = true
                  None
                case e: UnsupportedOperationException =>
                  HostLog.warn(s"completion handler for ${element.getLookupString} is unsupported", e)
                  CallLog.hit("Completion.filtered.unsupportedPlatform")
                  omitted = true
                  None
              }
            }
        }
      }.take(limit).toVector
      new CompletionList(omitted || items.size < sorted.size, items.asJava)
    }
  }

  /** Oracle replay selects one lookup identity without spending the interactive list's preview budget. */
  private[scalapsihost] def replay(file: ScalaFile, offset: Int, lookup: String,
                                  completionChar: Char = com.intellij.codeInsight.lookup.Lookup.NORMAL_SELECT_CHAR): CompletionItem = HostLock.modification {
    withResults(file, offset) { (document, results) =>
      val found = results.filter(_.getLookupElement.getAllLookupStrings.contains(lookup))
      require(found.nonEmpty, s"lookup absent: $lookup")
      val candidates = found.map { result =>
        unsupportedReason(result.getLookupElement).foreach(reason => throw new UnsupportedInsertion(reason))
        insert(file, offset, result, sort(results).map(_.getLookupElement).toArray, true, completionChar)
      }.distinct
      require(candidates.size == 1, s"lookup ambiguous: $lookup")
      val item = toItem(found.head, document, offset, 0)
      item.setTextEdit(LspEither.forLeft(candidates.head.primary))
      item.setAdditionalTextEdits(candidates.head.additional.asJava)
      item.setInsertTextFormat(InsertTextFormat.Snippet)
      item
    }
  }

  /** A recipe contains only snapshot and symbol identity; re-lookup must still select exactly one element. */
  private[scalapsihost] def recipe(item: CompletionItem): Recipe = {
    val gson = new com.google.gson.Gson
    val data = gson.toJsonTree(item.getData)
    require(data.isJsonObject, "completion resolve requires a recipe")
    val fields = data.getAsJsonObject
    require(Seq("uri", "version", "stamp", "offset", "lookup", "identity", "snippets").forall(fields.has), "incomplete completion recipe")
    Recipe(fields.get("uri").getAsString, fields.get("version").getAsInt, fields.get("stamp").getAsLong,
      fields.get("offset").getAsInt, fields.get("lookup").getAsString, fields.get("identity").getAsString, fields.get("snippets").getAsBoolean)
  }

  private[scalapsihost] def resolve(file: ScalaFile, item: CompletionItem, version: Int): CompletionItem = HostLock.modification {
    val selection = recipe(item)
    val document = file.getViewProvider.getDocument
    if (version != selection.version || document.getModificationStamp != selection.stamp)
      throw new com.intellij.openapi.progress.ProcessCanceledException
    withResults(file, selection.offset) { (_, results) =>
      val found = results.filter(r => r.getLookupElement.getLookupString == selection.lookup && identity(r) == selection.identity)
      require(found.size == 1, "completion resolve identity is absent or ambiguous")
      val insertion = insert(file, selection.offset, found.head, sort(results).map(_.getLookupElement).toArray, selection.snippets)
      require(item.getTextEdit != null && item.getTextEdit.isLeft && item.getTextEdit.getLeft == insertion.primary,
        "completion resolve would change the advertised primary edit")
      item.setAdditionalTextEdits(insertion.additional.asJava)
      item
    }
  }

  private def identity(result: CompletionResult): String = {
    val element = result.getLookupElement
    val psi = Option(element.getPsiElement)
    val path = psi.flatMap(e => Option(e.getContainingFile)).map(_.getViewProvider.getVirtualFile.getPath).getOrElse("")
    val signature = psi.map(e => Option(e.getText).getOrElse("")).getOrElse("")
    val evidence = Seq(element.getClass.getName, element.getLookupString, result.getPrefixMatcher.getPrefix, path,
      psi.map(_.getTextOffset).getOrElse(-1).toString, signature).mkString("\u0000")
    java.util.HexFormat.of().formatHex(java.security.MessageDigest.getInstance("SHA-256").digest(evidence.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
  }

  private def withResults[T](file: ScalaFile, offset: Int)(use: (Document, Vector[CompletionResult]) => T): T = {
    require(offset >= 0 && offset <= file.getTextLength, s"completion offset outside file: $offset")
    ProgressManager.checkCanceled()
    // ScalaPsiElementFactory builds synthetic PSI over an empty light file, without an editor document.
    val document = if (file.isPhysical) file.getViewProvider.getDocument
      else new com.intellij.openapi.editor.impl.DocumentImpl(file.getText)
    val editorState = new CompletionEditor(document, file.getProject, offset)
    val editor = editorState.editor
    val initializationFile = if (file.isPhysical) file else copyWithText(file, file.getText)
    val init = new CompletionInitializationContext(editor, editor.getCaretModel.getCurrentCaret, file.getLanguage,
      initializationFile, CompletionType.BASIC, 1)
    try {
      CompletionContributor.forLanguageHonorDumbness(file.getLanguage, file.getProject, editor).asScala.foreach { contributor =>
        ProgressManager.checkCanceled()
        contributor.beforeCompletion(init)
      }
      // OffsetsInFile.copyWithReplacement reparses through a PSI write transaction and document synchronization.
      // The completion tree is separate from both the original and every insertion preview.
      val text = file.getText
      val replacement = init.getDummyIdentifier
      val copy = copyWithText(file, text.substring(0, init.getStartOffset) + replacement + text.substring(init.getSelectionEndOffset))
      val copyOffsets = new OffsetMap(copy.getViewProvider.getDocument)
      for (key <- init.getOffsetMap.getAllOffsets.asScala) {
        val at = init.getOffsetMap.getOffset(key)
        val translated = if (at > offset || at == offset && key.isMovableToRight) at + replacement.length else at
        copyOffsets.addOffset(key, translated)
      }
      val copied = new OffsetsInFile(copy, copyOffsets)
      try {
        val translator = new OffsetTranslator(document, file, copied.getOffsets.getDocument,
          init.getStartOffset, init.getSelectionEndOffset, init.getDummyIdentifier)
        try {
          val parameters = CompletionInitializationUtil.createCompletionParameters(init, () => false, copied)
          val results = scala.collection.mutable.ArrayBuffer.empty[CompletionResult]
          CompletionService.getCompletionService.performCompletion(parameters, r => results += r)
          use(document, results.toVector)
        } finally Disposer.dispose(translator)
      } finally {
        copied.getOffsets.dispose()
        com.intellij.psi.impl.PsiManagerEx.getInstanceEx(file.getProject).getFileManager
          .setViewProvider(copy.getViewProvider.getVirtualFile, null)
      }
    } finally {
      init.getOffsetMap.dispose()
      editorState.close()
      if (initializationFile ne file) com.intellij.psi.impl.PsiManagerEx.getInstanceEx(file.getProject).getFileManager
        .setViewProvider(initializationFile.getViewProvider.getVirtualFile, null)
    }
  }

  private def unsupportedReason(element: LookupElement): Option[String] = {
    ScalaLookupItem.delegate(element) match {
      case builder: LookupElementBuilder => Option(builder.getInsertHandler).flatMap { handler =>
        handler match {
          case _: org.jetbrains.plugins.scala.lang.completion.lookups.ScalaKeywordLookupItem.KeywordInsertHandler => None
          case _ if handler.getClass.getName == "org.jetbrains.plugins.scala.lang.completion.handlers.ScalaConstructorInsertHandler" =>
            element.getPsiElement match {
              case cls: PsiClass if cls.isInterface || cls.hasModifierProperty(PsiModifier.ABSTRACT) => Some("implementationDialog")
              case _ => None
            }
          case _ => Some("handler." + handler.getClass.getName)
        }
      }
      case item: ScalaLookupItem => Option(item.getInsertHandler).flatMap { handler =>
        if (handler.isInstanceOf[org.jetbrains.plugins.scala.lang.completion.handlers.ScalaInsertHandler]) None
        else Some("handler." + handler.getClass.getName)
      }
      case other => Some("lookup." + other.getClass.getName)
    }
  }

  /** Only insertions whose handlers are identity operations may bypass the bounded preview. */
  private def plainInsertion(element: LookupElement): Boolean = element match {
    case item: ScalaLookupItem if item.getInsertHandler == null && !item.isClassName && !item.prefixCompletion &&
        !item.shouldImport && item.elementToImport.isEmpty && !item.someSmartCompletion && !item.isInSimpleString &&
        !item.isInInterpolatedString && !item.isNamedParameterOrAssignment && !item.isInStableElementPattern &&
        !item.getLookupString.contains("`") =>
      item.getPsiElement match {
        case function: ScFunction =>
          function.name != "classOf" && org.jetbrains.plugins.scala.lang.completion.handlers.ScalaInsertHandler
            .getItemParametersAndAccessorStatus(function)._1 < 0
        case function: org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.ScSyntheticFunction => function.paramClauses.isEmpty
        case _: PsiMethod | _: ScTypeDefinition => false
        case _ => true
      }
    case builder: LookupElementBuilder => Option(builder.getInsertHandler) match {
      case None => true
      case Some(handler: org.jetbrains.plugins.scala.lang.completion.lookups.ScalaKeywordLookupItem.KeywordInsertHandler) =>
        Set("this", "false", "true", "null", "super")(handler.keyword)
      case _ => false
    }
    case _ => false
  }

  /** The handler receives committed, original-text PSI and the outer lookup element, never the dummy identifier tree. */
  private def insert(file: ScalaFile, offset: Int, selected: CompletionResult, elements: Array[LookupElement], snippets: Boolean,
                     completionChar: Char = com.intellij.codeInsight.lookup.Lookup.NORMAL_SELECT_CHAR): Insertion = {
    val start = offset - selected.getPrefixMatcher.getPrefix.length
    var editorState = Option.empty[CompletionEditor]
    try {
      val original = file.getText
      val result = ChangeEngine.run(file) { copy =>
        val document = copy.getViewProvider.getDocument
        document.replaceString(start, offset, selected.getLookupElement.getLookupString)
        val tail = start + selected.getLookupElement.getLookupString.length
        val state = new CompletionEditor(document, copy.getProject, tail)
        editorState = Some(state)
        val offsets = new OffsetMap(document)
        offsets.addOffset(CompletionInitializationContext.START_OFFSET, start)
        offsets.addOffset(CompletionInitializationContext.SELECTION_END_OFFSET, tail)
        val identifierEnd = CompletionInitializationContext.calcDefaultIdentifierEnd(state.editor, tail)
        offsets.addOffset(CompletionInitializationContext.IDENTIFIER_END_OFFSET, identifierEnd)
        val context = new InsertionContext(offsets, completionChar,
          elements, copy, state.editor, false)
        try {
          if (completionChar == com.intellij.codeInsight.lookup.Lookup.REPLACE_SELECT_CHAR && identifierEnd > tail)
            document.deleteString(tail, identifierEnd)
          context.commitDocument()
          selected.getLookupElement.handleInsert(context)
          if (context.getLaterRunnable != null) throw new UnsupportedInsertion("laterRunnable")
        } finally offsets.dispose()
      }
      val caret = editorState.getOrElse(throw new IllegalStateException("handler editor was not created")).editor.getCaretModel.getOffset
      insertionEdits(original, start, offset, result, caret, snippets)
    } finally editorState.foreach(_.close())
  }

  /** Local diff hunks are coalesced with the prefix so LSP receives one single-line replacement containing its position. */
  private def insertionEdits(original: String, start: Int, offset: Int, result: ChangeEngine.Result,
                             caret: Int, snippets: Boolean): Insertion = {
    val document = new com.intellij.openapi.editor.impl.DocumentImpl(original, true)
    def at(p: Position): Int = document.getLineStartOffset(p.getLine) + p.getCharacter
    val line = document.getLineNumber(offset)
    var delta = 0
    var originalCaret = Option.empty[(Int, Int)]
    result.edits.foreach { edit =>
      val begin = at(edit.getRange.getStart)
      val end = at(edit.getRange.getEnd)
      val changedBegin = begin + delta
      val changedEnd = changedBegin + edit.getNewText.length
      if (originalCaret.isEmpty && caret <= changedEnd) {
        originalCaret = Some(if (caret < changedBegin) (caret - delta, caret - delta) else (begin, end))
      }
      delta += edit.getNewText.length - (end - begin)
    }
    val (caretStart, caretEnd) = originalCaret.getOrElse((caret - delta, caret - delta))
    var from = math.min(start, caretStart)
    var until = math.max(offset, caretEnd)
    var expanding = true
    while (expanding) {
      val touching = result.edits.filter(e => at(e.getRange.getEnd) >= from && at(e.getRange.getStart) <= until)
      val expandedStart = touching.map(e => at(e.getRange.getStart)).foldLeft(from)(math.min)
      val expandedEnd = touching.map(e => at(e.getRange.getEnd)).foldLeft(until)(math.max)
      expanding = expandedStart != from || expandedEnd != until
      from = expandedStart
      until = expandedEnd
    }
    if (from < document.getLineStartOffset(line) || until > document.getLineEndOffset(line))
      throw new UnsupportedInsertion("caretOutsidePrimary")
    val (local, additional) = result.edits.partition { edit =>
      at(edit.getRange.getEnd) >= from && at(edit.getRange.getStart) <= until
    }
    val beforeShift = additional.filter(e => at(e.getRange.getEnd) < from).map { e =>
      e.getNewText.length - (at(e.getRange.getEnd) - at(e.getRange.getStart))
    }.sum
    val localDelta = local.map(e => e.getNewText.length - (at(e.getRange.getEnd) - at(e.getRange.getStart))).sum
    val changedStart = from + beforeShift
    val changedEnd = until + beforeShift + localDelta
    val replacement = result.text.substring(changedStart, changedEnd)
    val caretInEdit = caret - changedStart
    if (caretInEdit < 0 || caretInEdit > replacement.length) throw new UnsupportedInsertion("caretOutsidePrimary")
    def position(at: Int): Position = new Position(line, at - document.getLineStartOffset(line))
    Insertion(new TextEdit(new Range(position(from), position(until)),
      if (snippets) snippet(replacement, caretInEdit) else replacement), additional)
  }

  private def snippet(text: String, caret: Int): String = {
    def escape(part: String): String = part.flatMap {
      case c @ ('\\' | '$' | '}') => "\\" + c
      case c => c.toString
    }
    escape(text.substring(0, caret)) + "$0" + escape(text.substring(caret))
  }

  /** Copies keep their original file for module scopes and their Scala features before the lazy tree is parsed. */
  private def copyWithText(file: ScalaFile, text: String): PsiFile = {
    val copy = PsiFileFactory.getInstance(file.getProject).createFileFromText(file.getName, file.getLanguage, text, false, false)
    copy match {
      case implementation: com.intellij.psi.impl.source.PsiFileImpl => implementation.setOriginalFile(file)
      case other => throw new IllegalStateException(s"completion copy is not a PSI source file: ${other.getClass.getName}")
    }
    ScalaFeatures.setAttachedScalaFeatures(copy, ScalaFeatures.forPsiOrDefault(file))
    copy
  }

  /** Each result retains its own sorter and matcher, including wrappers installed by Scala contributors. */
  private def sort(results: Vector[CompletionResult]): Vector[CompletionResult] = {
    val byElement = results.map(r => r.getLookupElement -> r).toMap
    val context = new ProcessingContext
    context.put(CompletionLookupArranger.PREFIX_CHANGES, Integer.valueOf(0))
    context.put(CompletionLookupArranger.WEIGHING_CONTEXT, new WeighingContext {
      override def itemMatcher(item: LookupElement): PrefixMatcher = byElement(item).getPrefixMatcher
      override def itemPattern(item: LookupElement): String = itemMatcher(item).getPrefix
    })
    val groups = scala.collection.mutable.LinkedHashMap.empty[CompletionSorterImpl, Vector[LookupElement]]
    for (result <- results) {
      val sorter = result.getSorter match {
        case s: CompletionSorterImpl => s
        case other => throw new IllegalStateException(s"unsupported completion sorter: ${other.getClass.getName}")
      }
      groups(sorter) = groups.getOrElse(sorter, Vector.empty) :+ result.getLookupElement
    }
    groups.toVector.flatMap { (sorter, elements) =>
      val classifier = sorter.buildClassifier(Classifier.empty[LookupElement]())
      // Stable source identity breaks equal-weight ties without collapsing overloads or depending on hash iteration.
      val stable = elements.sortBy { element =>
        val psi = Option(element.getPsiElement)
        val path = psi.flatMap(e => Option(e.getContainingFile)).map(_.getViewProvider.getVirtualFile.getPath).getOrElse("")
        val signature = psi match {
          case Some(f: org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.ScSyntheticFunction) =>
            f.paramClauses.map(_.map(_.paramType.canonicalText).mkString("(", ",", ")")).mkString + f.retType.canonicalText
          // Source elements are already told apart by file and offset; their text can be a whole method body.
          case _ => ""
        }
        (element.getLookupString, path, psi.map(_.getTextOffset).getOrElse(-1), signature)
      }
      stable.foreach { element =>
        ProgressManager.checkCanceled()
        classifier.addElement(element, context)
      }
      classifier.classify(stable.asJava, context).asScala.map(byElement).toVector
    }
  }

  private def toItem(result: CompletionResult, document: Document, offset: Int, rank: Int): CompletionItem = {
    val element = result.getLookupElement
    val presentation = new LookupElementPresentation
    element.renderElement(presentation)
    Option(element.getExpensiveRenderer).foreach { renderer =>
      // LookupElement's API returns an existential renderer; its contract is to render this very element.
      renderer.asInstanceOf[LookupElementRenderer[LookupElement]].renderElement(element, presentation)
    }
    val item = new CompletionItem(Option(presentation.getItemText).getOrElse(element.getLookupString))
    val details = new CompletionItemLabelDetails
    details.setDetail(presentation.getTailText)
    details.setDescription(presentation.getTypeText)
    item.setLabelDetails(details)
    item.setDetail(Seq(presentation.getTailText, presentation.getTypeText).filter(_ != null).map(_.trim).filter(_.nonEmpty).mkString(" "))
    item.setKind(kind(element))
    if (presentation.isStrikeout) item.setTags(java.util.List.of(CompletionItemTag.Deprecated))
    item.setSortText(f"$rank%06d")
    item.setFilterText(element.getLookupString)
    val insertion = element.getLookupString
    val start = offset - result.getPrefixMatcher.getPrefix.length
    require(start >= 0, "completion matcher prefix starts before the document")
    def position(at: Int): Position = {
      val line = document.getLineNumber(at)
      val location = LineColumn(line, at - document.getLineStartOffset(line))
      new Position(location.line, location.column)
    }
    item.setTextEdit(LspEither.forLeft(new TextEdit(new Range(position(start), position(offset)), insertion)))
    item
  }

  private def kind(item: LookupElement): CompletionItemKind = item.getPsiElement match {
    case _: ScEnumCase => CompletionItemKind.EnumMember
    case _: ScEnum => CompletionItemKind.Enum
    case _: ScTrait => CompletionItemKind.Interface
    case _: ScObject => CompletionItemKind.Module
    case _: ScTypeParam | _: PsiTypeParameter => CompletionItemKind.TypeParameter
    case _: ScTypeDefinition | _: ScTypeAlias | _: PsiClass => CompletionItemKind.Class
    case f: ScFunction if f.isConstructor => CompletionItemKind.Constructor
    case _: ScFunction | _: PsiMethod | _: org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.ScSyntheticFunction => CompletionItemKind.Method
    case _: ScClassParameter => CompletionItemKind.Field
    case _: ScParameter | _: PsiParameter => CompletionItemKind.Variable
    case _: PsiField => CompletionItemKind.Field
    case _: PsiPackage => CompletionItemKind.Module
    case named: ScNamedElement =>
      if (Option(item.as(classOf[ScalaLookupItem])).exists(_.isLocalVariable)) CompletionItemKind.Variable
      else named.nameContext match {
        case _: ScValueOrVariable => CompletionItemKind.Field
        case _ => CompletionItemKind.Value
      }
    case _ => item match {
      case builder: LookupElementBuilder if Option(builder.getInsertHandler).exists(
        _.isInstanceOf[org.jetbrains.plugins.scala.lang.completion.lookups.ScalaKeywordLookupItem.KeywordInsertHandler]) => CompletionItemKind.Keyword
      case _ => CompletionItemKind.Value
    }
  }
}
