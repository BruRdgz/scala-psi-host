package scalapsihost

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.codeInsight.intention.preview.{IntentionPreviewInfo, IntentionPreviewUtils}
import com.intellij.lang.surroundWith.SurroundDescriptor
import com.intellij.modcommand.{ActionContext, ModCommandExecutor}
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.util.TextRange
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.autoImport.quickFix.{MemberToImport, PrefixPackageToImport, ScalaImportTypeFix}
import org.jetbrains.plugins.scala.lang.psi.ScImportsHolder
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference

import scala.util.Using
import scala.util.control.NonFatal

object CodeActions {
  // These require the desktop rename UI or an inspection profile even to check availability.
  private val UnavailableIntentions = Set(
    "org.jetbrains.plugins.scala.codeInsight.intention.declarations.ChangeAccessModifierIntention",
    "org.jetbrains.plugins.scala.codeInsight.intention.expression.RemoveUnnecessaryParenthesesIntention",
  )

  private final case class Candidate(name: String, create: () => IntentionAction, kind: String)

  /** An action's edits, or the reason its preview cannot be applied as a source edit. */
  final case class Choice(title: String, kind: String, edits: Seq[ChangeEngine.Edit], disabled: Option[String])
}

/** Plugin intentions and annotator fixes, applied through the platform's preview contract to a file copy. */
final class CodeActions(intentionClasses: Seq[String], descriptorClasses: Seq[String]) {
  import CodeActions.{Candidate, Choice}

  def collect(file: ScalaFile, range: TextRange, problems: Seq[Diagnostic]): Seq[Choice] = {
    val project = file.getProject
    Using.resource(new RequestEditor(file.getViewProvider.getDocument, project, range.getStartOffset)) { request =>
      val editor = request.editor
      editor.getSelectionModel.setSelection(range.getStartOffset, range.getEndOffset)
      val intentions = intentionClasses.filterNot(CodeActions.UnavailableIntentions).map { name =>
        Candidate(name, () => Class.forName(name).getConstructor().newInstance().asInstanceOf[IntentionAction], "refactor.rewrite")
      }
      val fixes = problems.flatMap(_.fixes).collect {
        case (fixRange, fix) if fixRange.intersects(range) || fixRange.containsOffset(range.getStartOffset) =>
          Candidate(fix.getClass.getName, () => fix, "quickfix")
      }
      val choices = (fixes ++ intentions).flatMap { candidate =>
        var title = candidate.name.split('.').last
        var available = false
        try {
          val action = candidate.create()
          if (!action.isAvailable(project, editor, file)) None
          else {
            available = true
            title = action.getText
            val (result, info) = preview(file) { copy =>
              val previewEditor = IntentionPreviewUtils.getPreviewEditor
              previewEditor.getCaretModel.moveToOffset(range.getStartOffset)
              previewEditor.getSelectionModel.setSelection(range.getStartOffset, range.getEndOffset)
              val commandAction = action.asModCommandAction()
              if (commandAction == null) action.generatePreview(project, previewEditor, copy)
              else {
                val command = commandAction.perform(ActionContext.from(editor, file))
                ModCommandExecutor.getInstance.executeForFileCopy(command, copy)
                IntentionPreviewInfo.DIFF
              }
            }
            val supported = info == IntentionPreviewInfo.DIFF || info == IntentionPreviewInfo.DIFF_NO_TRIM
            Some(Choice(title, candidate.kind, if (supported) result.edits else Seq.empty,
              Option.when(!supported)("This action does not provide an applicable text preview.")))
          }
        } catch {
          case e: ProcessCanceledException => throw e
          case e @ (NonFatal(_) | _: LinkageError) =>
            HostLog.warn(s"code action $title could not be previewed", e)
            Option.when(available)(Choice(title, candidate.kind, Seq.empty, Some("This action is unavailable in the headless host.")))
        }
      }
      val surrounded = if (range.isEmpty) Seq.empty else descriptorClasses.flatMap { name =>
        val descriptor = Class.forName(name).getConstructor().newInstance().asInstanceOf[SurroundDescriptor]
        val elements = descriptor.getElementsToSurround(file, range.getStartOffset, range.getEndOffset)
        if (elements.isEmpty) Seq.empty else descriptor.getSurrounders.toSeq.flatMap { surrounder =>
          try {
            if (!surrounder.isApplicable(elements)) None
            else {
              val (result, _) = preview(file) { copy =>
                val selected = descriptor.getElementsToSurround(copy, range.getStartOffset, range.getEndOffset)
                val previewEditor = IntentionPreviewUtils.getPreviewEditor
                previewEditor.getCaretModel.moveToOffset(range.getStartOffset)
                previewEditor.getSelectionModel.setSelection(range.getStartOffset, range.getEndOffset)
                surrounder.surroundElements(project, previewEditor, selected)
              }
              Some(Choice(surrounder.getTemplateDescription, "refactor.rewrite", result.edits, None))
            }
          } catch {
            case e: ProcessCanceledException => throw e
            case e @ (NonFatal(_) | _: LinkageError) =>
              HostLog.warn(s"surround action ${surrounder.getClass.getName} could not be previewed", e)
              Some(Choice(surrounder.getTemplateDescription, "refactor.rewrite", Seq.empty,
                Some("This action is unavailable in the headless host.")))
          }
        }
      }
      (typeImports(file, range, problems) ++ choices ++ surrounded).distinct
    }
  }

  /** Offer each type-import candidate separately; the LSP client replaces the IDE's import chooser. */
  private def typeImports(file: ScalaFile, range: TextRange, problems: Seq[Diagnostic]): Seq[Choice] = {
    val references = problems.filter(p => p.range.intersects(range) || p.range.containsOffset(range.getStartOffset))
      .flatMap(p => Option(file.findReferenceAt(p.range.getStartOffset))).collect {
        case reference: ScReference if reference.qualifier.isEmpty && reference.multiResolveScala(false).isEmpty => reference
      }.distinct
    references.flatMap { reference =>
      ScalaImportTypeFix.getTypesToImport(reference).map { target =>
        val title = ScalaBundle.message("import.with", target.qualifiedName)
        try {
          val (result, _) = preview(file) { copy =>
            val copied = copy.findReferenceAt(reference.getTextRange.getStartOffset) match {
              case ref: ScReference => ref
              case _ => throw new IllegalStateException("the import reference is missing from the preview copy")
            }
            // The same binding operations as ScalaAddImportAction.ForReference, without its asynchronous chooser.
            target match {
              case PrefixPackageToImport(pack) => copied.bindToPackage(pack, addImport = true)
              case _: MemberToImport => ScImportsHolder(copied).addImportForPath(target.qualifiedName)
              case _ => copied.bindToElement(target.element)
            }
          }
          Choice(title, "quickfix", result.edits, None)
        } catch {
          case e: ProcessCanceledException => throw e
          case e @ (NonFatal(_) | _: LinkageError) =>
            HostLog.warn(s"code action $title could not be previewed", e)
            Choice(title, "quickfix", Seq.empty, Some("This import could not be previewed."))
        }
      }
    }
  }

  /** Logged platform failures can leave a partial preview even when the plugin returns normally. */
  private def preview[A](file: ScalaFile)(modify: ScalaFile => A): (ChangeEngine.Result, A) = {
    val errors = HostLog.count(HostLog.Level.Error)
    val result = ChangeEngine.run(file)(modify)
    if (HostLog.count(HostLog.Level.Error) != errors)
      throw new UnsupportedOperationException("An error was logged while previewing this action.")
    result
  }
}
