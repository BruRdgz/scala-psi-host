package scalapsihost

import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils
import com.intellij.diff.comparison.{ByCharRt, ByWordRt, CancellationChecker, ComparisonPolicy}
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.{Computable, TextRange}
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.codeStyle.{CodeStyleManager, CodeStyleSettingsManager}
import com.intellij.psi.impl.PsiManagerEx
import com.intellij.psi.impl.source.PostprocessReformattingAspect
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.project.ScalaFeatures

import scala.jdk.CollectionConverters.*

/**
 * Runs PSI changes on a nonphysical copy of a file, as the platform's intention preview does, and returns the
 * difference with the original as edits. The original file, its document and the indices are never touched.
 *
 * The write is acquired outside reads: a pending editor edit must be able to cancel the change, and a read-to-write
 * upgrade would deadlock the host.
 */
object ChangeEngine {

  /** A replacement of `range` of the original text by `text`. */
  final case class Edit(range: TextRange, text: String)

  /** The changed copy's text, and the edits that turn the original into it: sorted, disjoint and not touching. */
  final case class Result(text: String, edits: Seq[Edit])

  final class UnsupportedFormatter extends UnsupportedOperationException("scalafmt is not supported by scala-psi-host")

  /** The edits `modify` makes on a copy of `file`, and what it returns (e.g. state that follows the copy's document). */
  def run[A](file: ScalaFile)(modify: ScalaFile => A): (Result, A) = HostLock.modification {
    ProgressManager.checkCanceled()
    val original = file.getText
    val copy = previewCopy(file)
    val document = copy.getViewProvider.getDocument
    val documents = PsiDocumentManager.getInstance(file.getProject)
    val subscription = documents match {
      case manager: HostPsiDocumentManager => manager.listenTo(document)
      case other => throw new IllegalStateException(s"unexpected document manager: ${other.getClass.getName}")
    }
    try {
      val modified = PostprocessReformattingAspect.getInstance(file.getProject).postponeFormattingInside((() => {
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
      try ProgressManager.getInstance.executeNonCancelableSection((() =>
        documents.doPostponedOperationsAndUnblockDocument(document)): Runnable)
      finally {
        subscription.close()
        release(copy)
      }
    }
  }

  /**
   * Reformats `range` (the whole file without one) with the IntelliJ formatter, indenting by `indentSize` with spaces
   * or tabs. The settings are a request-local clone, so the request's indentation never alters the project's.
   */
  def format(file: ScalaFile, range: Option[TextRange], indentSize: Int, useTabs: Boolean): Result = HostLock.modification {
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

  /** Copies keep copyable user data and the original file, but Scala features live in plain user data. */
  private def previewCopy(file: ScalaFile): ScalaFile = {
    val copy = file.copy() match {
      case f: ScalaFile => f
      case other => throw new IllegalStateException(s"change copy is not a Scala file: ${other.getClass.getName}")
    }
    ScalaFeatures.setAttachedScalaFeatures(copy, ScalaFeatures.forPsiOrDefault(file))
    IntentionPreviewUtils.markAsPreviewCopy(copy, file)
    copy
  }

  /** The file manager keeps a copy's view provider until told otherwise (see commit e098341). */
  private def release(copy: ScalaFile): Unit =
    PsiManagerEx.getInstanceEx(copy.getProject).getFileManager.setViewProvider(copy.getViewProvider.getVirtualFile, null)
}
