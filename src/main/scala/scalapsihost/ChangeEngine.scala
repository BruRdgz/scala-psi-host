package scalapsihost

import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils
import com.intellij.diff.comparison.{ByCharRt, ByWordRt, CancellationChecker, ComparisonPolicy}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.{Computable, TextRange}
import com.intellij.psi.{PsiDocumentManager, PsiFile}
import com.intellij.psi.codeStyle.{CodeStyleManager, CodeStyleSettingsManager}
import com.intellij.psi.impl.PsiManagerEx
import com.intellij.psi.impl.source.PostprocessReformattingAspect
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.project.ScalaFeatures

import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * Runs PSI changes on a nonphysical copy of a file, as the platform's intention preview does, and returns the
 * difference with the original as edits. The original file, its document and the indices are never touched.
 *
 * As the platform's preview, a change runs in a read with an intention-preview session: queries go on meanwhile, the
 * plugin's write actions on the copy run in place (see the host's `runWriteAction`), and an editor edit cancels it.
 * Changes take turns among themselves: the platform's PSI-to-document synchronizer keeps the document it synchronizes
 * in one slot for the whole project (`PsiToDocumentSynchronizer.mySyncDocument`), copies included.
 */
object ChangeEngine {
  private val changesInTurn = new Object
  private val copyResources = new ThreadLocal[Using.Manager]

  /** A replacement of `range` of the original text by `text`. */
  final case class Edit(range: TextRange, text: String)

  /** The changed copy's text, and the edits that turn the original into it: sorted, disjoint and not touching. */
  final case class Result(text: String, edits: Seq[Edit])

  final class UnsupportedFormatter extends UnsupportedOperationException("scalafmt is not supported by scala-psi-host")

  /**
   * The edits `modify` makes on a copy of `file`, and what it returns (e.g. state that follows the copy's document).
   * `modify` runs in a preview session whose editor (`IntentionPreviewUtils.getPreviewEditor`) shows the copy.
   */
  def run[A](file: ScalaFile)(modify: ScalaFile => A): (Result, A) = HostLock.query {
    changesInTurn.synchronized {
      ProgressManager.checkCanceled()
      withPreviewCopy(file) { copy =>
        val document = copy.getViewProvider.getDocument
        Using.Manager { resources =>
          val previous = copyResources.get()
          copyResources.set(resources)
          try {
            resources(listenTo(copy))
            val editor = resources(new RequestEditor(document, copy.getProject, 0))
            inPreviewSession(editor.editor)(change(file.getText, copy, modify))
          } finally copyResources.set(previous)
        }.get
      }
    }
  }

  /** ModCommand creates further copies; their documents need the same commit listeners as the preview's file. */
  private[scalapsihost] def copyForModCommand(original: PsiFile): PsiFile = original match {
    case file: ScalaFile if copyResources.get() != null =>
      val resources = copyResources.get()
      val copy = file.copy() match {
        case f: ScalaFile => f
        case other => throw new IllegalStateException(s"mod-command copy is not a Scala file: $other")
      }
      resources((() => release(copy)): AutoCloseable)
      ScalaFeatures.setAttachedScalaFeatures(copy, ScalaFeatures.forPsiOrDefault(file))
      resources(listenTo(copy))
      copy
    case _ => null
  }

  /** `modify` on `copy`, its postponed formatting done and its document committed. */
  private def change[A](original: String, copy: ScalaFile, modify: ScalaFile => A): (Result, A) = {
    val document = copy.getViewProvider.getDocument
    val documents = PsiDocumentManager.getInstance(copy.getProject)
    try {
      val modified = PostprocessReformattingAspect.getInstance(copy.getProject).postponeFormattingInside((() => {
        val modified = modify(copy)
        documents.doPostponedOperationsAndUnblockDocument(document)
        modified
      }): Computable[A])
      documents.commitDocument(document)
      ProgressManager.checkCanceled()
      val changed = copy.getText
      require(document.getText == changed, "copy PSI and document diverged")
      (Result(changed, diff(original, changed)), modified)
    } finally {
      // Postponed tasks own range markers: drain them even when the change failed or was cancelled.
      ProgressManager.getInstance.executeNonCancelableSection((() =>
        documents.doPostponedOperationsAndUnblockDocument(document)): Runnable)
    }
  }

  private def listenTo(copy: ScalaFile): AutoCloseable = PsiDocumentManager.getInstance(copy.getProject) match {
    case manager: HostPsiDocumentManager => manager.listenTo(copy.getViewProvider.getDocument)
    case other => throw new IllegalStateException(s"unexpected document manager: ${other.getClass.getName}")
  }

  /** The platform's session runs a `Runnable`: the change's result comes back through a local. */
  private def inPreviewSession[A](editor: Editor)(body: => A): A = {
    var result = Option.empty[A]
    IntentionPreviewUtils.previewSession(editor, () => result = Some(body))
    result.getOrElse(throw new IllegalStateException("the preview session did not run the change"))
  }

  /**
   * `body` on a preview copy of `file`, released afterwards whatever fails. Copies keep copyable user data and the
   * original file, but Scala features live in plain user data.
   */
  private def withPreviewCopy[A](file: ScalaFile)(body: ScalaFile => A): A = {
    val copy = file.copy() match {
      case f: ScalaFile => f
      case other => throw new IllegalStateException(s"change copy is not a Scala file: ${other.getClass.getName}")
    }
    try {
      ScalaFeatures.setAttachedScalaFeatures(copy, ScalaFeatures.forPsiOrDefault(file))
      IntentionPreviewUtils.markAsPreviewCopy(copy, file)
      body(copy)
    } finally release(copy)
  }

  /**
   * Reformats `range` (the whole file without one) with the IntelliJ formatter, indenting by `indentSize` with spaces
   * or tabs. The settings are a request-local clone, so the request's indentation never alters the project's.
   */
  def format(file: ScalaFile, range: Option[TextRange], indentSize: Int, useTabs: Boolean): Result = HostLock.query {
    require(indentSize > 0, "the indent size must be positive")
    val manager = CodeStyleSettingsManager.getInstance(file.getProject)
    val settings = manager.getCurrentSettings.clone()
    if (settings.getCustomSettings(classOf[ScalaCodeStyleSettings]).USE_SCALAFMT_FORMATTER()) {
      HostLog.warn("scalafmt was selected, but scala-psi-host supports only the IntelliJ formatter")
      throw new UnsupportedFormatter
    }
    val indent = settings.getCommonSettings(ScalaLanguage.INSTANCE).getIndentOptions
    indent.TAB_SIZE = indentSize
    indent.INDENT_SIZE = indentSize
    indent.USE_TAB_CHARACTER = useTabs
    manager.computeWithLocalSettings(settings, _ => run(file) { copy =>
      val selected = range.getOrElse(copy.getTextRange)
      CodeStyleManager.getInstance(copy.getProject).reformatText(copy, selected.getStartOffset, selected.getEndOffset)
    }._1)
  }

  /**
   * The edits from `original` to `changed`: word hunks first, so an added import stays apart from a change in the
   * body, each refined by characters, so the unchanged whitespace around a change is kept.
   */
  private[scalapsihost] def diff(original: String, changed: String): Seq[Edit] = {
    val cancel: CancellationChecker = () => ProgressManager.checkCanceled()
    val edits = for {
      hunk <- ByWordRt.compare(original, changed, ComparisonPolicy.DEFAULT, cancel).asScala.toVector
      before = original.substring(hunk.getStartOffset1, hunk.getEndOffset1)
      after = changed.substring(hunk.getStartOffset2, hunk.getEndOffset2)
      change <- ByCharRt.compare(before, after, cancel).iterateChanges().asScala
    } yield Edit(
      new TextRange(hunk.getStartOffset1 + change.start1, hunk.getStartOffset1 + change.end1),
      after.substring(change.start2, change.end2))
    joinTouching(edits)
  }

  // Character changes of neighbouring words can meet: one edit per changed span keeps the edits apart.
  private def joinTouching(edits: Seq[Edit]): Seq[Edit] = edits.foldLeft(Vector.empty[Edit]) {
    case (joined :+ previous, edit) if previous.range.getEndOffset == edit.range.getStartOffset =>
      joined :+ Edit(previous.range.union(edit.range), previous.text + edit.text)
    case (joined, edit) => joined :+ edit
  }

  /** The file manager keeps a copy's view provider until told otherwise (see commit e098341). */
  private def release(copy: ScalaFile): Unit =
    PsiManagerEx.getInstanceEx(copy.getProject).getFileManager.setViewProvider(copy.getViewProvider.getVirtualFile, null)
}
