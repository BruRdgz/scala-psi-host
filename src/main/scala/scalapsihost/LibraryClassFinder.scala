package scalapsihost

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.impl.file.PsiPackageImpl
import com.intellij.psi.impl.java.stubs.index.JavaStubIndexKeys
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.stubs.StubIndex
import com.intellij.psi.{PsiClass, PsiClassOwner, PsiElementFinder, PsiJavaFile, PsiManager, PsiPackage}
import com.intellij.psi.impl.PsiManagerEx

import scala.jdk.CollectionConverters.*

/**
 * Classes and packages of the library roots, for `JavaPsiFacadeImpl` — the role the IDE's `PsiElementFinderImpl`
 * plays over its Java indexes:
 *
 *  - classes the plugin indexed under `JavaStubIndexKeys.CLASS_FQN` (Scala library classes and project sources, see [[SymbolIndex]]);
 *  - Java class files, looked up by path on demand instead of indexed, because the JDK alone has tens of
 *    thousands of them and resolution only ever asks for qualified names;
 *  - packages, which are the directories of the class roots.
 */
final class LibraryClassFinder(project: Project, files: LibraryFileSystem) extends PsiElementFinder {
  private def psiManager = PsiManager.getInstance(project)

  override def findClass(qualifiedName: String, scope: GlobalSearchScope): PsiClass =
    findClasses(qualifiedName, scope).headOption.orNull

  override def findClasses(qualifiedName: String, scope: GlobalSearchScope): Array[PsiClass] = {
    val indexed = StubIndex.getElements(JavaStubIndexKeys.CLASS_FQN, qualifiedName, project, scope, classOf[PsiClass]).asScala.toSeq
    (indexed ++ javaClassesByPath(qualifiedName, scope)).distinct.toArray
  }

  /** `java.util.Map.Entry`: directories `java/util`, class file `Map.class`, then inner class `Entry`. */
  private def javaClassesByPath(qualifiedName: String, scope: GlobalSearchScope): Seq[PsiClass] = {
    val segments = qualifiedName.split('.').toList
    for {
      root <- files.classRoots
      case (dir, topLevel :: innerNames) <- packageDirectories(root, segments)
      file <- Option(dir.findChild(topLevel + ".class")) if scope.contains(file)
      javaFile <- Option(psiManager.findFile(file)).collect { case f: PsiJavaFile => f }
      top <- javaFile.getClasses.find(_.getName == topLevel)
      cls <- innerNames.foldLeft(Option(top))((outer, name) => outer.flatMap(o => Option(o.findInnerClassByName(name, false))))
    } yield cls
  }

  /** Every split of `segments` into an existing package directory under `root` and a non-empty class path. */
  private def packageDirectories(root: VirtualFile, segments: List[String]): List[(VirtualFile, List[String])] =
    segments match {
      case Nil => Nil
      case head :: tail =>
        val here = List(root -> segments)
        Option(root.findChild(head)).filter(_.isDirectory) match {
          case Some(sub) => here ++ packageDirectories(sub, tail)
          case None => here
        }
    }

  private def directoriesOf(packageName: String): Seq[VirtualFile] = {
    val path = packageName.replace('.', '/')
    files.classRoots.flatMap(root => if (path.isEmpty) Some(root) else Option(root.findFileByRelativePath(path)).filter(_.isDirectory))
  }

  override def findPackage(qualifiedName: String): PsiPackage =
    if (directoriesOf(qualifiedName).isEmpty) null
    else new PsiPackageImpl(psiManager.asInstanceOf[PsiManagerEx], qualifiedName)

  override def getSubPackages(psiPackage: PsiPackage, scope: GlobalSearchScope): Array[PsiPackage] = {
    val prefix = if (psiPackage.getQualifiedName.isEmpty) "" else psiPackage.getQualifiedName + "."
    directoriesOf(psiPackage.getQualifiedName).flatMap(_.getChildren).filter(d => d.isDirectory && scope.contains(d))
      .map(_.getName).distinct.sorted
      .map(name => new PsiPackageImpl(psiManager.asInstanceOf[PsiManagerEx], prefix + name): PsiPackage).toArray
  }

  /** As the IDE: the classes of every class file in the package's directories that has a PSI file of its own. */
  override def getClasses(psiPackage: PsiPackage, scope: GlobalSearchScope): Array[PsiClass] =
    directoriesOf(psiPackage.getQualifiedName).flatMap(_.getChildren)
      .filter(f => !f.isDirectory && f.getName.endsWith(".class") && scope.contains(f))
      .flatMap(f => Option(psiManager.findFile(f)))
      .collect { case owner: PsiClassOwner => owner.getClasses.toSeq }
      .flatten.toArray
}
