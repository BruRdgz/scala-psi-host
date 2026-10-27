package scalapsihost

import com.intellij.lang.ASTNode
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.SimpleModificationTracker
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiElement, PsiFile, PsiJavaFile, PsiManager}
import com.intellij.psi.stubs.{IndexSink, ObjectStubSerializer, StubElement, StubElementRegistryService, StubIndex, StubIndexKey}
import com.intellij.util.Processor
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Outcome of indexing one library root.
 *
 * @param scalaIndexed top-level Scala class files whose decompiled PSI went into the index
 * @param javaClasses  Java class files: not indexed, the host's class finder reads them by path on demand
 * @param notTopLevel  class files without a PSI file of their own (inner and companion classes)
 * @param refused      Scala class files whose stubs could not be paired with their PSI; left out of the index
 */
final case class LibraryIndexReport(
  classFiles: Int,
  scalaIndexed: Int,
  javaClasses: Int,
  notTopLevel: Int,
  refused: Seq[String],
  occurrences: Int,
)

/**
 * The host's stub index: Scala files (decompiled library classes and project sources) go through the plugin's
 * stub builder and stub indexers into an in-memory map, which the host's [[StubIndex]] answers from. Entries are
 * kept per file, so a source file can be removed or replaced.
 */
final class SymbolIndex {
  private val elements = new ConcurrentHashMap[(String, AnyRef), java.util.List[PsiElement]]()
  private val keys = new ConcurrentHashMap[String, java.util.Set[AnyRef]]()
  /** Index name, key and element of every occurrence a file contributed. */
  private val entriesByFile = new ConcurrentHashMap[PsiFile, Vector[(String, AnyRef, PsiElement)]]()
  private val tracker = new SimpleModificationTracker

  // Index keys arrive as arbitrary CharSequences; lookups must compare them by content.
  private def normKey(k: AnyRef): AnyRef = k match {
    case cs: CharSequence => cs.toString
    case other => other
  }

  val stubIndex: StubIndex = new Host_StubIndex(dispatch {
    case ("StubIndex.getContainingFilesIterator", args) =>
      CallLog.hit(s"GAP index files ${args(0)} key=${args(1)}"); java.util.Collections.emptyIterator()
    case ("StubIndex.getAllKeys", Array(k: StubIndexKey[?, ?], _*)) =>
      java.util.List.copyOf(keys.getOrDefault(k.getName, java.util.Set.of()))
    case ("StubIndex.processElements", Array(k: StubIndexKey[?, ?], key, _, _, _, required: Class[?], proc: Processor[?])) =>
      val found = elements.get((k.getName, normKey(key)))
      if (found == null) { CallLog.hit(s"GAP index ${k.getName}"); java.lang.Boolean.TRUE }
      else {
        CallLog.hit(s"HIT index ${k.getName}")
        val p = proc.asInstanceOf[Processor[PsiElement]]
        java.lang.Boolean.valueOf(found.asScala.filter(required.isInstance).forall(p.process))
      }
    case ("StubIndex.processAllKeys", Array(k: StubIndexKey[?, ?], proc: Processor[?], _*)) =>
      val p = proc.asInstanceOf[Processor[AnyRef]]
      java.lang.Boolean.valueOf(keys.getOrDefault(k.getName, java.util.Set.of()).asScala.forall(p.process))
    case ("StubIndex.getMaxContainingFileCount", _) => Integer.valueOf(0)
    case ("StubIndex.getPerFileElementTypeModificationTracker" | "StubIndex.getStubIndexModificationTracker", _) => tracker
  })

  /**
   * Indexes the class files under `root` accepted by `includeEntry` (a path relative to the root, such as
   * `scala/Option.class`). PSI files come from the PsiManager, i.e. from the plugin's class-file decompiler.
   */
  def indexLibrary(project: Project, root: VirtualFile, includeEntry: String => Boolean): LibraryIndexReport = {
    val psiManager = PsiManager.getInstance(project)
    var classFiles, scalaIndexed, javaClasses, notTopLevel = 0
    val refused = mutable.ArrayBuffer.empty[String]
    def visit(dir: VirtualFile, relativeDir: String): Unit =
      for (file <- dir.getChildren) {
        val relative = relativeDir + file.getName
        if (file.isDirectory) visit(file, relative + "/")
        else if (file.getName.endsWith(".class") && includeEntry(relative)) {
          classFiles += 1
          psiManager.findFile(file) match {
            case scalaFile: ScalaFile => if (indexScalaFile(scalaFile)) scalaIndexed += 1 else refused += relative
            case _: PsiJavaFile => javaClasses += 1
            case _ => notTopLevel += 1
          }
        }
      }
    visit(root, "")
    val occurrences = entriesByFile.values().asScala.iterator.collect { case e if e.nonEmpty => e.size }.sum
    LibraryIndexReport(classFiles, scalaIndexed, javaClasses, notTopLevel, refused.toSeq, occurrences)
  }

  /**
   * Indexes one Scala file; returns false, indexing nothing, when its stubs cannot be paired with its PSI.
   * The indexed elements are the file's own PSI, so lookups land on the same tree callers hold.
   */
  def indexScalaFile(file: ScalaFile): Boolean = {
    val registry = StubElementRegistryService.Companion.getInstance()
    val builder = registry.getStubDescriptor(file.getLanguage).getStubDefinition.getBuilder
    def preorder(s: StubElement[?]): Seq[StubElement[?]] = s +: s.getChildrenStubs.asScala.toSeq.flatMap(preorder)
    val stubs = preorder(builder.buildStubTree(file)).drop(1)
    // Pair each stub with the AST element that produced it by mirroring the builder's traversal.
    val psis = mutable.ArrayBuffer.empty[PsiElement]
    def walk(node: ASTNode): Unit =
      node.getChildren(null).foreach { child =>
        if (!builder.skipChildProcessingWhenBuildingStubs(node, child)) {
          val factory = registry.getStubFactory(child.getElementType)
          if (factory != null && factory.shouldCreateStub(child)) psis += child.getPsi
          walk(child)
        }
      }
    walk(file.getNode)
    val paired = stubs.size == psis.size && stubs.zip(psis).forall((s, p) => s.getElementType == p.getNode.getElementType)
    // No mapping is better than a wrong one: an unpaired file stays out of the index.
    if (paired) {
      val entries = Vector.newBuilder[(String, AnyRef, PsiElement)]
      for ((stub, psi) <- stubs.zip(psis); serializer <- Option(registry.getStubSerializer(stub.getElementType))) {
        val sink = new IndexSink {
          override def occurrence[Psi <: PsiElement, K](key: StubIndexKey[K, Psi], value: K): Unit =
            entries += ((key.getName, normKey(value.asInstanceOf[AnyRef]), psi))
        }
        serializer.asInstanceOf[ObjectStubSerializer[StubElement[?], ?]].indexStub(stub, sink)
      }
      val fileEntries = entries.result()
      for ((index, key, psi) <- fileEntries) {
        elements.computeIfAbsent((index, key), _ => new CopyOnWriteArrayList[PsiElement]()).add(psi)
        keys.computeIfAbsent(index, _ => ConcurrentHashMap.newKeySet[AnyRef]()).add(key)
      }
      entriesByFile.put(file, fileEntries)
      tracker.incModificationCount()
    }
    paired
  }

  /** Removes everything `file` contributed. */
  def removeFile(file: PsiFile): Unit = {
    val fileEntries = entriesByFile.remove(file)
    if (fileEntries != null) {
      for ((index, key, psi) <- fileEntries) {
        val list = elements.get((index, key))
        if (list != null) {
          list.remove(psi)
          if (list.isEmpty) {
            elements.remove((index, key))
            keys.get(index).remove(key)
          }
        }
      }
      tracker.incModificationCount()
    }
  }
}
