package scalapsihost

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiFile
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.util.indexing.FileContentImpl
import org.jetbrains.plugins.scala.autoImport.ImportOrderingIndexer

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** Import qualifiers indexed by the plugin, used by its ordering of auto-import candidates. */
final class ImportIndex(project: Project) {
  private val indexer = new ImportOrderingIndexer().getIndexer
  private val importsByFile = new ConcurrentHashMap[VirtualFile, Set[String]]()

  def indexFile(file: PsiFile): Unit = {
    val virtualFile = file.getViewProvider.getVirtualFile
    val imports = indexer.map(FileContentImpl.createByText(virtualFile, file.getText, project))
    importsByFile.put(virtualFile, imports.keySet.asScala.toSet)
  }

  def removeFile(file: PsiFile): Unit = importsByFile.remove(file.getViewProvider.getVirtualFile)

  /** A Void index has one distinct value (null) when any file of the scope contains the key. */
  def values(qualifier: String, scope: GlobalSearchScope): java.util.List[Void] =
    if (importsByFile.asScala.exists((file, imports) => scope.contains(file) && imports(qualifier)))
      java.util.Collections.singletonList(null)
    else java.util.List.of()
}
