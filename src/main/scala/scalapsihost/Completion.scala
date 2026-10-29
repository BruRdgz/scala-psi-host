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

/** BASIC completion through the plugin's contributors; insertion stays a plain, request-local text edit. */
object Completion {
  // Bounds rendering and the JSON response while leaving enough candidates for interactive prefix refinement.
  val MaxItems = 200

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

  private[scalapsihost] def complete(file: ScalaFile, offset: Int, limit: Int = MaxItems): CompletionList = {
    require(offset >= 0 && offset <= file.getTextLength, s"completion offset outside file: $offset")
    ProgressManager.checkCanceled()
    // ScalaPsiElementFactory builds synthetic PSI over an empty light file, without an editor document.
    val document = if (file.isPhysical) file.getViewProvider.getDocument
      else new com.intellij.openapi.editor.impl.DocumentImpl(file.getText)
    val editor = CompletionEditor(document, file.getProject, offset)
    val initializationFile = if (file.isPhysical) file else copyWithText(file, file.getText)
    val init = new CompletionInitializationContext(editor, editor.getCaretModel.getCurrentCaret, file.getLanguage,
      initializationFile, CompletionType.BASIC, 1)
    try {
      CompletionContributor.forLanguageHonorDumbness(file.getLanguage, file.getProject, editor).asScala.foreach { contributor =>
        ProgressManager.checkCanceled()
        contributor.beforeCompletion(init)
      }
      // OffsetsInFile.copyWithReplacement reparses through a PSI write transaction and document synchronization.
      // A fresh non-physical tree keeps this request a read and never mutates the original document.
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
          val usable = results.filter { result =>
            val item = result.getLookupElement
            val scalaItem = Option(item.as(classOf[ScalaLookupItem]))
            val needsImport = scalaItem.exists(i => i.shouldImport || i.elementToImport.nonEmpty || i.prefixCompletion || i.isClassName)
            // Constructor builders can bind/import a class without ScalaLookupItem's shouldImport flag.
            val needsHandler = Option(item.as(classOf[LookupElementBuilder])).flatMap(b => Option(b.getInsertHandler)).exists(
              !_.isInstanceOf[org.jetbrains.plugins.scala.lang.completion.lookups.ScalaKeywordLookupItem.KeywordInsertHandler])
            val needsInsertion = needsHandler || scalaItem.exists(i => i.someSmartCompletion || i.isInSimpleString || i.isInInterpolatedString)
            if (needsImport) CallLog.hit("Completion.filtered.import")
            if (needsInsertion) CallLog.hit("Completion.filtered.insertion")
            !needsImport && !needsInsertion
          }.toVector
          val sorted = sort(usable)
          val items = sorted.take(limit).zipWithIndex.map { (result, rank) =>
            ProgressManager.checkCanceled()
            toItem(result, document, offset, rank)
          }
          ProgressManager.checkCanceled()
          new CompletionList(sorted.size > limit, items.asJava)
        } finally Disposer.dispose(translator)
      } finally copied.getOffsets.dispose()
    } finally init.getOffsetMap.dispose()
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
    val named = Option(element.as(classOf[ScalaLookupItem])).exists(_.isNamedParameter)
    val insertion = element.getLookupString + (if (named) " = " else "")
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
