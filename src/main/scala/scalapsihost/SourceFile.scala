package scalapsihost

import com.intellij.openapi.fileTypes.FileType
import com.intellij.testFramework.LightVirtualFile

import java.nio.file.Path

/**
 * A project source file: its text lives in memory (the editor's buffer, or the file's content when opened), and its
 * path is the file's real path. Edits go through its document (see [[ScalaPsiHost.edit]]); nothing is written to disk.
 */
final class SourceFile(val path: Path, fileType: FileType, text: String)
  extends LightVirtualFile(path.getFileName.toString, fileType, text) {
  // Sources are UTF-8, as scalac reads them by default; the IDE's per-file encoding settings do not exist here.
  setCharset(java.nio.charset.StandardCharsets.UTF_8)

  override def getPath: String = path.toString

  // No assigned language, unlike other light files (e.g. the plugin's REPL input): as for an IDE file, the language
  // comes from the file type through language substitution, which picks Scala 3 for a Scala 3 module's file.
  override def getLanguage: com.intellij.lang.Language = null
}
