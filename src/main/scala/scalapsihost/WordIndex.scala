package scalapsihost

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiFile
import com.intellij.psi.impl.cache.impl.id.{IdIndexEntry, IdTableBuilding}
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.util.indexing.FileContentImpl

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** Files that contain every word of `words`, each with an occurrence mask `accepts` takes (see [[WordIndex.filesWithAll]]). */
final case class WordQuery(words: Seq[IdIndexEntry], accepts: Int => Boolean)

/**
 * The host's word index, the platform's `IdIndex`: for each project source file, the words it contains (as
 * [[IdIndexEntry]] hashes, case-sensitive and not) and the contexts they occur in (code, comments, strings; the
 * masks of `UsageSearchContext`). The platform's reference search asks it which files to scan for a name. Entries
 * are kept per file, so a source file can be removed or replaced.
 */
final class WordIndex(project: Project) {
  private val wordsByFile = new ConcurrentHashMap[VirtualFile, java.util.Map[IdIndexEntry, Integer]]()
  private val filesByWord = new ConcurrentHashMap[IdIndexEntry, java.util.Set[VirtualFile]]()

  /** Indexes `file`'s current text with the platform's indexer for its file type (the language's words scanner). */
  def indexFile(file: PsiFile): Unit = {
    val virtualFile = file.getViewProvider.getVirtualFile
    val indexer = IdTableBuilding.getFileTypeIndexer(virtualFile.getFileType)
    if (indexer == null) throw new IllegalStateException(s"no word indexer for ${virtualFile.getFileType}")
    val words = indexer.map(FileContentImpl.createByText(virtualFile, file.getText, project))
    wordsByFile.put(virtualFile, words)
    for (word <- words.keySet.asScala)
      filesByWord.computeIfAbsent(word, _ => ConcurrentHashMap.newKeySet[VirtualFile]()).add(virtualFile)
  }

  /** Removes everything `file` contributed. */
  def removeFile(file: PsiFile): Unit = {
    val virtualFile = file.getViewProvider.getVirtualFile
    val words = wordsByFile.remove(virtualFile)
    if (words != null)
      for (word <- words.keySet.asScala) {
        val files = filesByWord.get(word)
        files.remove(virtualFile)
        if (files.isEmpty) filesByWord.remove(word)
      }
  }

  /** The contexts `word` occurs in within `file`; none when the file is not indexed or lacks the word. */
  def occurrences(word: IdIndexEntry, file: VirtualFile): Option[Int] =
    Option(wordsByFile.get(file)).flatMap(words => Option(words.get(word))).map(_.intValue)

  def filesWith(word: IdIndexEntry, scope: GlobalSearchScope): Seq[VirtualFile] =
    Option(filesByWord.get(word)).fold(Seq.empty[VirtualFile])(_.asScala.filter(scope.contains).toSeq)

  /** The files of `scope` in which every query is satisfied. With no word to look for nothing matches, as in the IDE's index. */
  def filesWithAll(queries: Seq[WordQuery], scope: GlobalSearchScope): Seq[VirtualFile] =
    queries.headOption.flatMap(_.words.headOption).fold(Seq.empty[VirtualFile]) { first =>
      filesWith(first, scope).filter { file =>
        queries.forall(query => query.words.forall(word => occurrences(word, file).exists(query.accepts)))
      }
    }
}
