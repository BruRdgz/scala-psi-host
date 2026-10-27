package scalapsihost

import com.intellij.lang.ASTNode
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.SimpleModificationTracker
import com.intellij.psi.{PsiElement, PsiFileFactory}
import com.intellij.psi.stubs.{IndexSink, ObjectStubSerializer, StubElement, StubElementRegistryService, StubIndex, StubIndexKey}
import com.intellij.util.Processor
import org.jetbrains.plugins.scala.ScalaFileType
import org.jetbrains.plugins.scala.decompiler.Decompiler

import java.nio.file.Path
import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * Outcome of indexing one library jar.
 *
 * @param refused class files whose stubs could not be paired with their PSI; they are left out of the index
 * @param skipped class files the host deliberately does not index yet (see [[LibraryIndex.knownUnsupported]])
 */
final case class LibraryIndexReport(
  classFiles: Int,
  notScala: Int,
  indexed: Int,
  refused: Seq[String],
  skipped: Seq[String],
  occurrences: Int,
)

/**
 * The host's symbol index: library binaries go through the plugin's decompiler, stub builder and stub
 * indexers into an in-memory map, which the host's [[StubIndex]] answers from.
 */
final class LibraryIndex {
  private val elements = new ConcurrentHashMap[(String, AnyRef), java.util.List[PsiElement]]()
  private val keys = new ConcurrentHashMap[String, java.util.Set[AnyRef]]()
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

  /** Indexes the `.class` entries of `jar` accepted by `includeEntry` (a jar entry name such as `scala/Option.class`). */
  def indexJar(project: Project, jar: Path, includeEntry: String => Boolean): LibraryIndexReport = {
    val registry = StubElementRegistryService.Companion.getInstance()
    var notScala, indexed, occurrences = 0
    val refused, skipped = mutable.ArrayBuffer.empty[String]
    val entryNames = Using.resource(new ZipFile(jar.toFile)) { zip =>
      val names = zip.entries().asScala.map(_.getName).filter(n => n.endsWith(".class") && includeEntry(n)).toVector
      for (name <- names) {
        if (LibraryIndex.knownUnsupported.contains(name)) skipped += name
        else {
          val bytes = zip.getInputStream(zip.getEntry(name)).readAllBytes()
          // Only top-level Scala classes decompile to a source file; inner classes and Java classes do not.
          Decompiler.sourceNameAndText(name.substring(name.lastIndexOf('/') + 1), bytes) match {
            case None => notScala += 1
            case Some((sourceName, text)) =>
              val file = PsiFileFactory.getInstance(project).createFileFromText(sourceName, ScalaFileType.INSTANCE, text)
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
              if (!paired) refused += name
              else {
                indexed += 1
                for ((stub, psi) <- stubs.zip(psis); serializer <- Option(registry.getStubSerializer(stub.getElementType))) {
                  val sink = new IndexSink {
                    override def occurrence[Psi <: PsiElement, K](key: StubIndexKey[K, Psi], value: K): Unit = {
                      occurrences += 1
                      val k = normKey(value.asInstanceOf[AnyRef])
                      elements.computeIfAbsent((key.getName, k), _ => new CopyOnWriteArrayList[PsiElement]()).add(psi)
                      keys.computeIfAbsent(key.getName, _ => ConcurrentHashMap.newKeySet[AnyRef]()).add(k)
                    }
                  }
                  serializer.asInstanceOf[ObjectStubSerializer[StubElement[?], ?]].indexStub(stub, sink)
                }
              }
          }
        }
      }
      names
    }
    tracker.incModificationCount()
    LibraryIndexReport(entryNames.size, notScala, indexed, refused.toSeq, skipped.toSeq, occurrences)
  }
}

object LibraryIndex {
  /**
   * `scala.Array` is also a synthetic class; resolving through it navigates to its library source directory
   * (`SyntheticNamedElement`, ScSyntheticClass.scala:84) and dereferences it unchecked. The host has no library
   * directory model until phase F1, so this class file is not indexed yet.
   */
  val knownUnsupported: Set[String] = Set("scala/Array.class")
}
