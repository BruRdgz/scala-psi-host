package scalapsihost.platform

import com.intellij.lang.ASTNode
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.SimpleModificationTracker
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiElement, PsiFile, PsiJavaFile, PsiManager}
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.stubs.{IndexSink, ObjectStubSerializer, StubElement, StubElementRegistryService, StubIndex, StubIndexKey}
import com.intellij.util.Processor
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

import scalapsihost.LibraryIndexReport

import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * The host's stub index: Scala files (decompiled library classes and project sources) go through the plugin's
 * stub builder and stub indexers into an in-memory map, which the host's `StubIndex` answers from. Entries are
 * kept per file, so a source file can be removed or replaced.
 */
final class SymbolIndex {
  // The IDE filters index postings by file before visiting PSI, without repeating each element's PSI navigation.
  /** Keeps each indexed PSI element paired with its source file for scope filtering and removal. */
  private final case class IndexedElement(file: VirtualFile, psi: PsiElement)

  private val elements = new ConcurrentHashMap[(String, AnyRef), java.util.List[IndexedElement]]()
  private val keys = new ConcurrentHashMap[String, java.util.Set[AnyRef]]()
  /** Index name, key and element of every occurrence a file contributed. */
  private val entriesByFile = new ConcurrentHashMap[PsiFile, Vector[(String, AnyRef, IndexedElement)]]()
  private val tracker = new SimpleModificationTracker

  // Index keys arrive as arbitrary CharSequences; lookups must compare them by content.
  /** Converts index CharSequence keys to stable string keys for content-based lookup. */
  private def normKey(k: AnyRef): AnyRef = k match {
    case cs: CharSequence => cs.toString
    case other => other
  }

  val stubIndex: StubIndex = new Host_StubIndex(dispatch {
    case ("StubIndex.getContainingFilesIterator", args) =>
      CallLog.hit(s"GAP index files ${args(0)} key=${args(1)}"); java.util.Collections.emptyIterator()
    case ("StubIndex.getAllKeys", Array(k: StubIndexKey[?, ?], _*)) =>
      java.util.List.copyOf(keys.getOrDefault(k.getName, java.util.Set.of()))
    case ("StubIndex.processElements", Array(k: StubIndexKey[?, ?], key, _, scope, _, required: Class[?], proc: Processor[?])) =>
      val found = elements.get((k.getName, normKey(key)))
      if (found == null) { CallLog.hit(s"GAP index ${k.getName}"); java.lang.Boolean.TRUE }
      else {
        CallLog.hit(s"HIT index ${k.getName}")
        // Only elements of files in the caller's scope (a module's dependencies), as the IDE's index filters them.
        def inScope(entry: IndexedElement) = scope match {
          case s: GlobalSearchScope => s.contains(entry.file)
          case _ => true
        }
        val p = proc.asInstanceOf[Processor[PsiElement]]
        val candidates = found.asScala.iterator.filter(entry => required.isInstance(entry.psi) && inScope(entry))
        java.lang.Boolean.valueOf(candidates.forall(entry => p.process(entry.psi)))
      }
    case ("StubIndex.processAllKeys", Array(k: StubIndexKey[?, ?], proc: Processor[?], _*)) =>
      val p = proc.asInstanceOf[Processor[AnyRef]]
      java.lang.Boolean.valueOf(keys.getOrDefault(k.getName, java.util.Set.of()).asScala.forall(p.process))
    case ("StubIndex.getMaxContainingFileCount", _) => Integer.valueOf(0)
    case ("StubIndex.getPerFileElementTypeModificationTracker" | "StubIndex.getStubIndexModificationTracker", _) => tracker
  })

  /**
   * Indexes the class and TASTy files under `root` accepted by `includeEntry` (a path relative to the root, such as
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
        else if ((file.getName.endsWith(".class") || file.getName.endsWith(".tasty")) && includeEntry(relative)) {
          classFiles += 1
          psiManager.findFile(file) match {
            case scalaFile: ScalaFile => if (indexLibraryFile(scalaFile)) scalaIndexed += 1 else refused += relative
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
   * Indexes a decompiled library file from its stub tree, as the IDE keeps library PSI: the indexed elements are
   * stub-based, and the file's syntax tree is built only if something navigates into it. Returns false, indexing
   * nothing, when the file has no stub tree to read (its syntax tree is already loaded).
   */
  private def indexLibraryFile(file: ScalaFile): Boolean =
    file match {
      case stubbed: com.intellij.psi.impl.source.PsiFileImpl =>
        Option(stubbed.getStubTree) match {
          case Some(tree) =>
            // The first stub is the file's own.
            record(file, tree.getPlainList.asScala.toSeq.drop(1).map(stub => stub -> stub.getPsi))
            true
          case None => false
        }
      case _ => false
    }

  /**
   * Indexes one source file (Scala or Java); returns false, indexing nothing, when its stubs cannot be paired with
   * its PSI. The indexed elements are the file's own PSI, so lookups land on the same tree callers hold.
   */
  def indexSourceFile(file: PsiFile): Boolean = {
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
    if (paired) record(file, stubs.zip(psis))
    paired
  }

  /** Runs the stub indexers over each stub and records the occurrences for its element, as `file`'s entries. */
  private def record(file: PsiFile, stubsWithElements: Seq[(StubElement[?], PsiElement)]): Unit = {
    val registry = StubElementRegistryService.Companion.getInstance()
    val entries = Vector.newBuilder[(String, AnyRef, IndexedElement)]
    val virtualFile = file.getViewProvider.getVirtualFile
    for ((stub, psi) <- stubsWithElements; serializer <- Option(registry.getStubSerializer(stub.getElementType))) {
      val element = IndexedElement(virtualFile, psi)
      val sink = new IndexSink {
        override def occurrence[Psi <: PsiElement, K](key: StubIndexKey[K, Psi], value: K): Unit =
          entries += ((key.getName, normKey(value.asInstanceOf[AnyRef]), element))
      }
      serializer.asInstanceOf[ObjectStubSerializer[StubElement[?], ?]].indexStub(stub, sink)
    }
    val fileEntries = entries.result()
    for ((index, key, element) <- fileEntries) {
      elements.computeIfAbsent((index, key), _ => new CopyOnWriteArrayList[IndexedElement]()).add(element)
      keys.computeIfAbsent(index, _ => ConcurrentHashMap.newKeySet[AnyRef]()).add(key)
    }
    entriesByFile.put(file, fileEntries)
    tracker.incModificationCount()
  }

  /** Removes everything `file` contributed. */
  def removeFile(file: PsiFile): Unit = {
    val fileEntries = entriesByFile.remove(file)
    if (fileEntries != null) {
      for ((index, key, element) <- fileEntries) {
        val list = elements.get((index, key))
        if (list != null) {
          list.remove(element)
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
