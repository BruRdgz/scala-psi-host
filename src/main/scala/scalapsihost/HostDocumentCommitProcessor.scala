package scalapsihost

import com.intellij.openapi.application.ModalityState
import com.intellij.openapi.editor.Document
import com.intellij.openapi.progress.EmptyProgressIndicator
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiDocumentManager, PsiFile}
import com.intellij.psi.impl.{BlockSupportImpl, BooleanRunnable, ChangedPsiRangeUtil, DocumentCommitKind, DocumentCommitProcessor, PsiDocumentManagerBase, PsiDocumentManagerEx}

import scala.jdk.CollectionConverters.*

/**
 * Document commits as the IDE's DocumentCommitThread makes them, synchronously only (the host commits on the editing
 * thread, inside its write action): for each PSI file of the document, the range that changed since the last commit
 * is reparsed, and the resulting difference is applied to the existing tree, which sends the tree change events.
 */
final class HostDocumentCommitProcessor extends DocumentCommitProcessor {
  override def commitSynchronously(document: Document, project: Project, psiFile: PsiFile): Unit = {
    val documents = PsiDocumentManager.getInstance(project).asInstanceOf[PsiDocumentManagerBase]
    val lastCommitted = documents.getLastCommittedText(document)
    val text = document.getImmutableCharSequence
    val applyChanges = for {
      viewProvider <- documents.getCachedViewProviders(document).asScala.toSeq
      file <- viewProvider.getAllFiles.asScala.toSeq
      changed <- Option(ChangedPsiRangeUtil.getChangedPsiRange(file, document, lastCommitted, text))
    } yield {
      val reparsed = BlockSupportImpl.reparse(file, file.getNode, changed, text, new EmptyProgressIndicator, lastCommitted)
      (() => { reparsed.log.doActualPsiChange(file); true }): BooleanRunnable
    }
    documents.finishCommit(document, applyChanges.asJava, java.util.List.of(), DocumentCommitKind.Synchronous.INSTANCE, "host edit")
  }

  override def commitAsynchronously(project: Project, documents: PsiDocumentManagerEx, document: Document, reason: Any,
                                    modality: ModalityState): Unit =
    CallLog.unsupported("DocumentCommitProcessor.commitAsynchronously (the host commits synchronously)")

  override def commitSynchronouslyLightweight(document: Document, project: Project): Unit =
    CallLog.unsupported("DocumentCommitProcessor.commitSynchronouslyLightweight")
}
