package scalapsihost.language

import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.Computable
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.openapi.project.Project
import com.intellij.psi.impl.light.LightElement
import com.intellij.psi.impl.source.PostprocessReformattingAspect
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile}
import com.intellij.refactoring.listeners.RefactoringElementListener
import com.intellij.refactoring.rename.{PsiElementRenameHandler, RenamePsiElementProcessor, RenameUtil}
import com.intellij.refactoring.util.{MoveRenameUsageInfo, RelatedUsageInfo}
import com.intellij.usageView.UsageInfo
import com.intellij.util.containers.MultiMap

import scalapsihost.platform.SourceFile

import java.util

import scala.jdk.CollectionConverters.*

/**
 * The IDE's rename of a symbol, without its dialogs: the plugin's rename processors choose the elements to rename (a
 * class with its companion, a member with its overriders, a var with its setter...) and find their references, then
 * update them as `RenameProcessor` does. A conflict the IDE would show in its dialog refuses the rename instead.
 * Optional automatic renames (variables named after a class, overloads, parameters of overriders) and renames of
 * files are left out.
 */
object Rename {
  /** Why the symbol cannot be renamed, in the platform's words. */
  final class Refused(val reasons: Seq[String]) extends RuntimeException(reasons.mkString("\n"))

  /** What a rename changes: each element with its new name, the references to update, and the project files they are
   * in.
   */
  final class Plan private[Rename] (val project: Project,
                                    val renames: util.LinkedHashMap[PsiElement, String],
                                    val usages: Array[UsageInfo]) {
    val files: Seq[PsiFile] = (renames.keySet.asScala.flatMap(e => Option(e.getContainingFile)) ++ usages.map(_.getFile))
      .filter(_.getViewProvider.getVirtualFile.isInstanceOf[SourceFile]).toSeq.distinct
  }

  // The plugin's processors ask where the IDE has a popup or dialog (rename the base member, rename the getters and
  // setters too): unit-test mode is their way of answering with the defaults, and the host has no UI to ask.
  private val answeringDialogs = ThreadLocal.withInitial(() => false)

  /** Whether rename processors should use their unit-test defaults instead of requesting UI choices. */
  def isAnsweringDialogs: Boolean = answeringDialogs.get

  /** Temporarily makes processor questions resolve to the host's deterministic defaults on this thread. */
  private def withDefaultAnswers[A](body: => A): A = {
    val previous = answeringDialogs.get
    answeringDialogs.set(true)
    try body
    finally answeringDialogs.set(previous)
  }

  /** The element the IDE renames for the symbol at `offset`, when it can be renamed. */
  def target(file: PsiFile, offset: Int): PsiElement = {
    val found = ProjectSearch.usageTarget(file, offset).getOrElse(throw new Refused(Seq("There is no symbol to rename here.")))
    if (PsiElementRenameHandler.isVetoed(found)) throw new Refused(Seq("This element cannot be renamed."))
    val substituted = withDefaultAnswers(RenamePsiElementProcessor.forElement(found).substituteElementToRename(found, null))
    if (substituted == null) throw new Refused(Seq("This element cannot be renamed."))
    // Only source files belong to the editable workspace; attached sources remain read-only.
    if (!Option(substituted.getContainingFile).exists(_.getViewProvider.getVirtualFile.isInstanceOf[SourceFile]))
      throw new Refused(Seq("This element is not located inside the project."))
    Option(PsiElementRenameHandler.getRenameErrorMessage(file.getProject, null, substituted)).foreach(m => throw new Refused(Seq(m)))
    substituted
  }

  /** The elements and references of renaming `target` to `newName`; refused for an invalid name or a conflict. */
  def plan(target: PsiElement, newName: String): Plan = {
    val project = target.getProject
    if (!RenameUtil.isValidName(project, target, newName)) throw new Refused(Seq(s"'$newName' is not a valid name for this element."))
    val renames = new util.LinkedHashMap[PsiElement, String]
    renames.put(target, newName)
    withDefaultAnswers(RenamePsiElementProcessor.allForElement(target).asScala.foreach(_.prepareRenaming(target, newName, renames)))
    // The file of a top-level class follows its name in the IDE; the host's files have fixed paths.
    renames.keySet.removeIf(_.isInstanceOf[PsiFile])
    val scope = GlobalSearchScope.projectScope(project)
    val usages = renames.asScala.toSeq.flatMap { (element, name) =>
      RenameUtil.findUsages(element, name, scope, false, false, renames)
    }.toArray
    val conflicts = new MultiMap[PsiElement, String]
    RenameUtil.addConflictDescriptions(usages, conflicts)
    RenamePsiElementProcessor.forElement(target).findExistingNameConflicts(target, newName, conflicts, renames)
    if (!conflicts.isEmpty) throw new Refused(conflicts.values.asScala.toSeq.distinct)
    new Plan(project, renames, usages)
  }

  /** Renames the plan's elements and updates their references, in the PSI of the project's files. */
  def perform(plan: Plan): Unit = {
    ProgressManager.checkCanceled()
    val classified = classify(plan)
    val ignored = new RefactoringElementListener {
      override def elementMoved(newElement: PsiElement): Unit = ()
      override def elementRenamed(newElement: PsiElement): Unit = ()
    }
    val documents = PsiDocumentManager.getInstance(plan.project)
    // The IDE formats what a rename changed once its command ends.
    val callbacks = PostprocessReformattingAspect.getInstance(plan.project).postponeFormattingInside((() => {
      val callbacks = plan.renames.asScala.toSeq.flatMap { (element, name) =>
        val processor = RenamePsiElementProcessor.forElement(element)
        val usages = classified.getOrElse(element, Seq.empty).asJava
        val callback = processor.getPostRenameCallback(element, name, usages, plan.renames, ignored)
        processor.renameElement(element, name, usages.toArray(UsageInfo.EMPTY_ARRAY), ignored)
        Option(callback)
      }
      plan.files.foreach(f => documents.doPostponedOperationsAndUnblockDocument(documents.getDocument(f)))
      callbacks
    }): Computable[Seq[Runnable]])
    plan.files.foreach(f => documents.commitDocument(documents.getDocument(f)))
    callbacks.foreach(_.run())
  }

  /**
   * The usages by the renamed element each updates, as `RenameProcessor.classifyUsages` does (the class itself needs
   * the usage view): by the element a usage refers to, or its navigation element; implicit references are skipped.
   */
  private def classify(plan: Plan): Map[PsiElement, Seq[UsageInfo]] = {
    val renamed = plan.renames.keySet.asScala
    /** Resolves a usage to an element in the planned rename set, following navigation when needed. */
    def renamedElement(usage: MoveRenameUsageInfo): Option[PsiElement] = usage match {
      case related: RelatedUsageInfo => Some(related.getRelatedElement).filter(renamed)
      case _ => Option(usage.getReferencedElement).flatMap { element =>
        if (renamed(element)) Some(element) else Option(element.getNavigationElement).filter(renamed)
      }
    }
    plan.usages.toSeq.collect { case usage: MoveRenameUsageInfo if !usage.getReference.isInstanceOf[LightElement] => usage }
      .flatMap(usage => renamedElement(usage).map(_ -> (usage: UsageInfo)))
      .groupMap(_._1)(_._2)
  }
}
