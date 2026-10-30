package scalapsihost

import com.intellij.codeInsight.TargetElementUtil
import com.intellij.openapi.project.Project
import com.intellij.psi.codeStyle.NameUtil
import com.intellij.psi.search.{GlobalSearchScope, PsiShortNamesCache}
import com.intellij.psi.search.searches.DefinitionsScopedSearch
import com.intellij.psi.{PsiElement, PsiFile, PsiNamedElement}
import com.intellij.usageView.UsageInfo
import com.intellij.util.CommonProcessors
import org.jetbrains.plugins.scala.extensions.PsiNamedElementExt
import org.jetbrains.plugins.scala.findUsages.factory.ScalaFindUsagesHandlerFactory
import org.jetbrains.plugins.scala.gotoclass.{ScalaGoToClassContributor, ScalaGoToSymbolContributor}

import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * What the IDE searches the whole project for from an editor position or a typed name: the usages of the symbol
 * there (find usages), its implementations (go to implementation) and the symbols whose names match (go to symbol).
 * All over the project's sources (see [[ModuleModel.sourcesScope]]); callers hold the host's read lock.
 */
object ProjectSearch {
  /** A symbol found by name, with the qualifier the plugin's go-to contributor gives it (a package or an enclosing class). */
  final case class FoundSymbol(element: PsiNamedElement, container: Option[String])

  /**
   * The most symbols a name query answers. An editor lists a screenful and the user types on to narrow the query, so
   * the best matches are what counts; the cap also bounds the work of looking items up by name.
   */
  val MaxSymbols = 100

  /** The symbol the editor takes `offset` to be about for find usages: the one referenced there, or the one declared there. */
  def usageTarget(file: PsiFile, offset: Int): Option[PsiElement] =
    targetAt(file, offset, TargetElementUtil.getInstance.getReferenceSearchFlags)

  // The host shows no lookup, so the lookup item is never a target (and the lookup manager does not exist).
  private def targetAt(file: PsiFile, offset: Int, flags: Int): Option[PsiElement] =
    Using.resource(new RequestEditor(file.getViewProvider.getDocument, file.getProject, offset)) { editor =>
      Option(TargetElementUtil.getInstance.findTargetElement(editor.editor, flags & ~TargetElementUtil.LOOKUP_ITEM_ACCEPTED, offset))
    }

  /**
   * The usages of `target`, as the plugin's find-usages handler reports them with its default options: those of the
   * handler's primary and secondary elements. None for a symbol the plugin's handler does not take (a Java declaration).
   */
  def usages(target: PsiElement): Seq[UsageInfo] = {
    val factory = new ScalaFindUsagesHandlerFactory(target.getProject)
    if (!factory.canFindUsages(target)) Seq.empty
    else {
      val handler = factory.createFindUsagesHandler(target, false)
      val collected = new CommonProcessors.CollectProcessor[UsageInfo]
      for (element <- handler.getPrimaryElements ++ handler.getSecondaryElements)
        handler.processElementUsages(element, collected, handler.getFindUsagesOptions)
      collected.getResults.asScala.toSeq
    }
  }

  /**
   * What go to implementation shows for the symbol at `offset`: its implementations and overriders, as the IDE's
   * handler finds them. From a reference with none (or to a concrete member, which the plugin's evaluator includes), the
   * symbol itself.
   */
  def implementations(file: PsiFile, offset: Int): Seq[PsiElement] = {
    val util = TargetElementUtil.getInstance
    val flags = util.getDefinitionSearchFlags
    targetAt(file, offset, flags).toSeq.flatMap { source =>
      val scope = GlobalSearchScope.projectScope(file.getProject)
      val others = DefinitionsScopedSearch.search(source, scope, /* checkDeep = */ true).findAll().asScala.toSeq
      val onReference = targetAt(file, offset, flags & ~TargetElementUtil.REFERENCED_ELEMENT_ACCEPTED).isEmpty
      if (onReference && util.includeSelfInGotoImplementation(source)) source +: others
      else if (onReference && others.isEmpty) Seq(source)
      else others
    }
  }

  /**
   * The project's classes and other symbols whose names match `query`, best match first, at most [[MaxSymbols]]. The
   * plugin's go-to contributors (and the short-names cache, for classes) list the names and the items of a name; the
   * query matches names as the IDE's go to symbol does: camel humps and word starts, anywhere in the name. A query with dots (`pkg.Name`) matches the name by its last
   * part and the item's qualified name by the whole. A blank query matches nothing.
   */
  def symbols(project: Project, query: String): Seq[FoundSymbol] =
    if (query.isBlank) Seq.empty
    else {
      val symbolContributor = new ScalaGoToSymbolContributor
      val contributors = Seq(new ScalaGoToClassContributor, symbolContributor)
      // Most classes are visible to Java, so not in the contributors' indexes: the IDE's class contributor lists them from
      // the short-names cache, which is read here directly (the platform's contributor needs more of the IDE than the host has).
      val shortNames = PsiShortNamesCache.getInstance(project)
      val scope = GlobalSearchScope.projectScope(project)
      val nameMatcher = NameUtil.buildMatcher("*" + query.substring(query.lastIndexOf('.') + 1)).build()
      val qualifiedMatcher = NameUtil.buildMatcher("*" + query).build()
      val names = (contributors.flatMap(_.getNames(project, false)) ++ shortNames.getAllClassNames).distinct
        .filter(nameMatcher.matches).sortBy(name => (-nameMatcher.matchingDegree(name), name))
      // A name can belong to library symbols only, so the cap applies to the items found, not to the names tried.
      names.iterator
        .flatMap(name => contributors.flatMap(_.getItemsByName(name, query, project, false)) ++ shortNames.getClassesByName(name, scope))
        .collect { case named: PsiNamedElement => named }.distinct
        .map(named => named -> symbolContributor.getQualifiedName(named))
        .filter((_, qualified) => !query.contains('.') || qualified != null && qualifiedMatcher.matches(qualified))
        .take(MaxSymbols)
        .map((named, qualified) => FoundSymbol(named, container(qualified, named.name)))
        .toSeq
    }

  private def container(qualifiedName: String, name: String): Option[String] =
    Option(qualifiedName).filter(_.endsWith("." + name)).map(_.stripSuffix("." + name))
}
