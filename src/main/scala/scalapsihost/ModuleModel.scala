package scalapsihost

import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.search.GlobalSearchScope

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/**
 * A build target as a build server describes it (BSP `buildTarget/scalacOptions`): the Scala version, the compiler
 * options and the compile classpath (which includes scala-library), and the targets it depends on.
 */
final case class ModuleSpec(
  name: String,
  scalaVersion: String,
  scalacOptions: Seq[String],
  classpath: Seq[Path],
  dependsOn: Seq[String] = Seq.empty,
)

/**
 * One module of the host's project: the IDE `Module` object the plugin sees, the library roots of its classpath and
 * the source files opened in it.
 */
final class HostModule private[scalapsihost] (val spec: ModuleSpec, val libraryRoots: Set[VirtualFile]) {
  private[scalapsihost] var module: Module = scala.compiletime.uninitialized
  private[scalapsihost] val sources = ConcurrentHashMap.newKeySet[VirtualFile]()
}

/**
 * The host's modules and which module each source file belongs to. Resolution from a module's file sees the
 * module's own sources, those of the modules it depends on (transitively), its classpath and the JDK — as the IDE's
 * module-with-dependencies scope.
 */
final class ModuleModel(project: Project) {
  private val modules = java.util.Collections.synchronizedMap(new java.util.LinkedHashMap[String, HostModule]())
  private val moduleOfFile = new ConcurrentHashMap[VirtualFile, HostModule]()
  @volatile private var jdkRoots: Set[VirtualFile] = Set.empty

  def all: Seq[HostModule] = modules.synchronized(modules.values().asScala.toVector)
  def named(name: String): Option[HostModule] = Option(modules.get(name))
  def ofFile(file: VirtualFile): Option[HostModule] = Option(moduleOfFile.get(file))
  def ofModule(module: Module): Option[HostModule] = all.find(_.module eq module)

  private[scalapsihost] def setJdkRoots(roots: Seq[VirtualFile]): Unit = jdkRoots = roots.toSet

  /** The mounted JDK's module roots. */
  private[scalapsihost] def jdkModuleRoots: Set[VirtualFile] = jdkRoots

  private[scalapsihost] def add(hostModule: HostModule): Unit = {
    val missing = hostModule.spec.dependsOn.filterNot(modules.containsKey)
    if (missing.nonEmpty) throw new IllegalArgumentException(s"module ${hostModule.spec.name} depends on unknown modules: ${missing.mkString(", ")}")
    if (modules.putIfAbsent(hostModule.spec.name, hostModule) != null)
      throw new IllegalArgumentException(s"module ${hostModule.spec.name} already exists")
  }

  /** Removes a module whose sources were removed; modules that depend on it must be removed or added again too. */
  private[scalapsihost] def remove(name: String): Unit = {
    val removed = Option(modules.remove(name)).getOrElse(throw new IllegalArgumentException(s"no module named $name"))
    if (!removed.sources.isEmpty) throw new IllegalStateException(s"module $name still has sources")
    // Scopes of the modules that depend on it reach it.
    scopes.clear()
  }

  private[scalapsihost] def addSource(file: VirtualFile, hostModule: HostModule): Unit = {
    hostModule.sources.add(file)
    moduleOfFile.put(file, hostModule)
  }

  private[scalapsihost] def removeSource(file: VirtualFile): Unit =
    Option(moduleOfFile.remove(file)).foreach(_.sources.remove(file))

  /** The module and the modules it depends on, transitively, each once. */
  private def withDependencies(hostModule: HostModule): Seq[HostModule] = {
    val seen = scala.collection.mutable.LinkedHashSet.empty[HostModule]
    def visit(m: HostModule): Unit = if (seen.add(m)) m.spec.dependsOn.flatMap(named).foreach(visit)
    visit(hostModule)
    seen.toSeq
  }

  // One scope object per module, as the IDE's ModuleWithDependenciesScope: the plugin keys its caches by scope,
  // so a new object per request would make every lookup miss and the caches grow without bound.
  private val scopes = new ConcurrentHashMap[HostModule, GlobalSearchScope]()

  def resolveScope(hostModule: HostModule): GlobalSearchScope = scopes.computeIfAbsent(hostModule, newScope)

  // A module's dependencies are fixed when it is added; its sources and the JDK roots are read on each check.
  private def newScope(hostModule: HostModule): GlobalSearchScope = new GlobalSearchScope(project) {
    private val reachable = withDependencies(hostModule)
    private val roots = reachable.flatMap(_.libraryRoots).toSet

    override def contains(file: VirtualFile): Boolean = file match {
      case library: LibraryFile => roots.contains(rootOf(library)) || jdkRoots.contains(rootOf(library))
      case source => reachable.exists(_.sources.contains(source))
    }
    override def isSearchInModuleContent(module: Module): Boolean = reachable.exists(_.module eq module)
    override def isSearchInLibraries: Boolean = true
    override def toString: String = s"ModuleWithDependencies(${hostModule.spec.name})"
  }

  private def rootOf(file: VirtualFile): VirtualFile = {
    var f = file
    while (f.getParent != null) f = f.getParent
    f
  }
}
