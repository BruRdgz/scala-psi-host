package scalapsihost

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.impl.file.PsiPackageImpl
import com.intellij.psi.impl.java.stubs.index.JavaStubIndexKeys
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.stubs.StubIndex
import com.intellij.psi.{PsiClass, PsiClassOwner, PsiElementFinder, PsiJavaFile, PsiManager, PsiPackage}
import com.intellij.psi.impl.PsiManagerEx
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScPackaging
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.stubs.index.{ScPackageObjectFqnIndex, ScPackagingFqnIndex, ScalaIndexKeys}

import scala.jdk.CollectionConverters.*

/**
 * Classes and packages of the library roots, for `JavaPsiFacadeImpl` — the role the IDE's `PsiElementFinderImpl`
 * plays over its Java indexes:
 *
 *  - classes the plugin indexed under `JavaStubIndexKeys.CLASS_FQN` (Scala library classes and project sources, see [[SymbolIndex]]);
 *  - Java class files, looked up by path on demand instead of indexed, because the JDK alone has tens of
 *    thousands of them and resolution only ever asks for qualified names;
 *  - packages, from the class-root directories and indexed Scala package declarations.
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
    if (directoriesOf(qualifiedName).isEmpty && !isDeclaredPackage(qualifiedName)) null
    else new PsiPackageImpl(psiManager.asInstanceOf[PsiManagerEx], qualifiedName)

  /**
   * A package that Scala sources declare (a `package` clause or a package object), as the IDE's finder sees source
   * packages: otherwise the plugin's synthetic packages stand in, and they list subpackages of every module.
   */
  private def isDeclaredPackage(qualifiedName: String): Boolean = {
    val everywhere = GlobalSearchScope.allScope(project)
    ScPackagingFqnIndex.instance.hasElement(qualifiedName, project, everywhere, classOf[ScPackaging]) ||
      ScPackageObjectFqnIndex.instance.hasElement(qualifiedName, project, everywhere, classOf[PsiClass])
  }

  override def getSubPackages(psiPackage: PsiPackage, scope: GlobalSearchScope): Array[PsiPackage] = {
    val packageName = psiPackage.getQualifiedName
    val prefix = if (packageName.isEmpty) "" else packageName + "."
    val directories = directoriesOf(packageName).flatMap(_.getChildren)
      .filter(d => d.isDirectory && scope.contains(d)).map(_.getName)
    (directories ++ declaredSubPackages(packageName, prefix, scope)).distinct.sorted
      .map(name => new PsiPackageImpl(psiManager.asInstanceOf[PsiManagerEx], prefix + name): PsiPackage).toArray
  }

  /** The names of the subpackages of `packageName` that sources in `scope` declare: nested packagings, package objects. */
  private def declaredSubPackages(packageName: String, prefix: String, scope: GlobalSearchScope): Seq[String] = {
    // A packaging is indexed under its own name only: the root package's subpackages are those of every name.
    val packageNames =
      if (packageName.isEmpty) StubIndex.getInstance.getAllKeys(ScalaIndexKeys.PACKAGE_FQN_KEY, project).asScala.toSeq.map(_.toString)
      else Seq(packageName)
    packageNames.flatMap(ScPackagingFqnIndex.instance.getElements(_, project, scope).asScala).flatMap { packaging =>
      val name = packaging.fullPackageName
      if (name == packageName) packaging.immediateTypeDefinitions.collect { case o: ScObject if o.isPackageObjectNonLegacy => o.name }
      else if (name.startsWith(prefix)) Seq(name.stripPrefix(prefix).takeWhile(_ != '.'))
      else Seq.empty
    }
  }

  /** As the IDE: the classes of every class file in the package's directories that has a PSI file of its own. */
  override def getClasses(psiPackage: PsiPackage, scope: GlobalSearchScope): Array[PsiClass] =
    directoriesOf(psiPackage.getQualifiedName).flatMap(_.getChildren)
      .filter(f => !f.isDirectory && f.getName.endsWith(".class") && scope.contains(f))
      .flatMap(f => Option(psiManager.findFile(f)))
      .collect { case owner: PsiClassOwner => owner.getClasses.toSeq }
      .flatten.toArray
}
