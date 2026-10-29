package scalapsihost

import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils
import com.intellij.diff.comparison.{ByCharRt, ByWordRt, CancellationChecker, ComparisonPolicy}
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.impl.DocumentImpl
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.codeStyle.{CodeStyleManager, CodeStyleSettingsManager}
import com.intellij.psi.impl.source.PostprocessReformattingAspect
import org.eclipse.lsp4j.{FormattingOptions, Position, Range, TextEdit}
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.project.ScalaFeatures

import scala.jdk.CollectionConverters.*

/**
 * Runs PSI changes on a nonphysical copy. The write is acquired outside reads because a pending editor edit must
 * be able to cancel this computation, and a read-to-write upgrade would deadlock the host.
 */
object ChangeEngine {
  final case class Result(snapshotStamp: Long, text: String, edits: Seq[TextEdit])
  private final case class OffsetEdit(start: Int, end: Int, text: String)

  final class UnsupportedFormatter extends UnsupportedOperationException("scalafmt is not supported by scala-psi-host")

  def run(file: ScalaFile)(modify: ScalaFile => Unit): Result = HostLock.modification {
    ProgressManager.checkCanceled()
    val snapshot = file.getText
    val documents = PsiDocumentManager.getInstance(file.getProject)
    val stamp = Option(documents.getCachedDocument(file)).fold(file.getModificationStamp)(_.getModificationStamp)
    val copy = file.copy() match {
      case f: ScalaFile => f
      case other => throw new IllegalStateException(s"change copy is not a Scala file: ${other.getClass.getName}")
    }
    ScalaFeatures.setAttachedScalaFeatures(copy, ScalaFeatures.forPsiOrDefault(file))
    IntentionPreviewUtils.markAsPreviewCopy(copy, file)
    val provider = copy.getViewProvider
    val document = provider.getDocument
    val subscription = documents match {
      case manager: HostPsiDocumentManager => manager.listenTo(document)
      case other => throw new IllegalStateException(s"unexpected document manager: ${other.getClass.getName}")
    }
    val aspect = PostprocessReformattingAspect.getInstance(file.getProject)
    try {
      aspect.postponeFormattingInside((() => {
        modify(copy)
        documents.doPostponedOperationsAndUnblockDocument(document)
      }): Runnable)
      documents.commitDocument(document)
      ProgressManager.checkCanceled()
      val changed = copy.getText
      require(document.getText == changed, "copy PSI and document diverged")
      Result(stamp, changed, diff(snapshot, changed))
    } finally {
      // Postponed tasks own range markers. Drain them even if modification or cancellation failed.
      try ProgressManager.getInstance.executeNonCancelableSection((() =>
        documents.doPostponedOperationsAndUnblockDocument(document)): Runnable)
      finally {
        subscription.close()
        com.intellij.psi.impl.PsiManagerEx.getInstanceEx(file.getProject).getFileManager
          .setViewProvider(provider.getVirtualFile, null)
      }
    }
  }

  /** Request settings are thread-local clones, so tab options never alter the project's defaults. */
  def format(file: ScalaFile, range: Option[TextRange], options: FormattingOptions): Result = HostLock.modification {
    require(options.getTabSize > 0, "formatting tabSize must be positive")
    val manager = CodeStyleSettingsManager.getInstance(file.getProject)
    val settings = manager.getCurrentSettings.clone()
    if (settings.getCustomSettings(classOf[ScalaCodeStyleSettings]).USE_SCALAFMT_FORMATTER()) {
      HostLog.warn("scalafmt was selected, but scala-psi-host supports only the IntelliJ formatter")
      throw new UnsupportedFormatter
    }
    val indent = settings.getCommonSettings(ScalaLanguage.INSTANCE).getIndentOptions
    indent.TAB_SIZE = options.getTabSize
    indent.INDENT_SIZE = options.getTabSize
    indent.USE_TAB_CHARACTER = !options.isInsertSpaces
    manager.computeWithLocalSettings(settings, _ => run(file) { copy =>
      val selected = range.getOrElse(copy.getTextRange)
      CodeStyleManager.getInstance(copy.getProject).reformatText(copy, selected.getStartOffset, selected.getEndOffset)
    })
  }

  /** Word boundaries keep imports separate from body changes; character refinement retains unchanged whitespace. */
  private[scalapsihost] def diff(original: String, changed: String): Seq[TextEdit] = {
    val snapshot = new DocumentImpl(original, true)
    val cancel: CancellationChecker = () => ProgressManager.checkCanceled()
    val hunks = ByWordRt.compare(original, changed, ComparisonPolicy.DEFAULT, cancel).asScala
    val offsets = hunks.toVector.flatMap { hunk =>
      val before = original.substring(hunk.getStartOffset1, hunk.getEndOffset1)
      val after = changed.substring(hunk.getStartOffset2, hunk.getEndOffset2)
      ByCharRt.compare(before, after, cancel).iterateChanges().asScala.map { change =>
        val start = hunk.getStartOffset1 + change.start1
        val end = hunk.getStartOffset1 + change.end1
        OffsetEdit(start, end, after.substring(change.start2, change.end2))
      }
    }
    val disjoint = scala.collection.mutable.ArrayBuffer.empty[OffsetEdit]
    offsets.foreach { edit =>
      disjoint.lastOption match {
        case Some(previous) =>
          require(previous.end <= edit.start, "diff returned overlapping or unsorted changes")
          if (previous.end == edit.start) disjoint(disjoint.size - 1) = OffsetEdit(previous.start, edit.end, previous.text + edit.text)
          else disjoint += edit
        case None => disjoint += edit
      }
    }
    disjoint.toVector.map(edit => new TextEdit(new Range(position(snapshot, edit.start), position(snapshot, edit.end)), edit.text))
  }

  private def position(document: Document, offset: Int): Position = {
    val line = document.getLineNumber(offset)
    val location = LineColumn(line, offset - document.getLineStartOffset(line))
    new Position(location.line, location.column)
  }
}
