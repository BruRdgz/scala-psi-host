package scalapsihost.platform

import com.intellij.openapi.editor.{Caret, CaretModel, Document, Editor, EditorSettings, SelectionModel}
import com.intellij.openapi.editor.colors.EditorColorsManager
import com.intellij.openapi.editor.event.{DocumentEvent, DocumentListener}
import com.intellij.openapi.editor.ex.util.LexerEditorHighlighter
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.{Key, TextRange, UserDataHolderBase}
import com.intellij.psi.PsiDocumentManager
import org.jetbrains.plugins.scala.highlighter.ScalaSyntaxHighlighterFactory

/**
 * A request-local editor that tracks caret and selection positions through document and PSI changes.
 * Offsets use UTF-16 code units; selection starts are inclusive and ends exclusive.
 *
 * The caller owns this editor and must close it to detach its document listener. Use it on the thread running
 * the request, under the same read/preview or write action as the document operations.
 */
private[scalapsihost] final class RequestEditor(document: Document, project: Project, offset: Int)
    extends AutoCloseable {
  private val data = new UserDataHolderBase
  private var caretOffset = offset
  private var selectionStart = offset
  private var selectionEnd = offset
  private lazy val settings = tableProxy(classOf[EditorSettings], "SelectionSettings") {
    // AbstractWordSelectioner and ScalaStringLiteralSelectioner read the platform's default camel-word mode.
    case ("isCamelWords", _) => java.lang.Boolean.FALSE
  }
  // Selection queries do not edit the document; the highlighter lexes its request-local snapshot.
  private lazy val highlighter = {
    val file = PsiDocumentManager.getInstance(project).getPsiFile(document)
    val syntax = ScalaSyntaxHighlighterFactory.createScalaSyntaxHighlighter(project, file.getVirtualFile, file.getLanguage)
    val result = new LexerEditorHighlighter(syntax, EditorColorsManager.getInstance.getGlobalScheme)
    result.setText(document.getImmutableCharSequence)
    result
  }
  private val listener = new DocumentListener {
    override def documentChanged(event: DocumentEvent): Unit = {
      def shifted(at: Int, right: Boolean): Int =
        if (at < event.getOffset || at == event.getOffset && !right) at
        else if (at > event.getOffset + event.getOldLength) at + event.getNewLength - event.getOldLength
        else event.getOffset + event.getNewLength
      val hadSelection = selectionStart != selectionEnd
      caretOffset = shifted(caretOffset, true)
      selectionStart = shifted(selectionStart, false)
      selectionEnd = shifted(selectionEnd, true)
      if (!hadSelection || selectionStart == selectionEnd) removeSelection()
    }
  }
  document.addDocumentListener(listener)

  private def move(at: Int): Unit = {
    require(at >= 0 && at <= document.getTextLength, s"caret outside document: $at")
    caretOffset = at
    removeSelection()
  }
  private def removeSelection(): Unit = { selectionStart = caretOffset; selectionEnd = caretOffset }
  private def select(start: Int, end: Int): Unit = {
    require(start >= 0 && start <= end && end <= document.getTextLength, "selection outside document")
    selectionStart = start
    selectionEnd = end
  }

  lazy val editor: Editor = tableProxy(classOf[Editor], "RequestEditor") {
    case ("getDocument", _) => document
    case ("getProject", _) => project
    case ("getCaretModel", _) => model
    case ("getSelectionModel", _) => selection
    // Word selection reads isCamelWords; it never changes editor settings.
    case ("getSettings", _) => settings
    // EditorActionUtil's lexeme boundaries and InjectedFileReferenceSelectioner's escapes use the layered lexer.
    case ("getHighlighter", _) => highlighter
    // A source editor, rather than a one-line input field.
    case ("isOneLineMode", _) => java.lang.Boolean.FALSE
    // The host has no editor-specific color scheme.
    case ("getColorsScheme", _) => EditorColorsManager.getInstance.getGlobalScheme
    case ("getUserData", Array(k: Key[?])) => data.getUserData(k).asInstanceOf[AnyRef]
    case ("putUserData", Array(k: Key[?], value)) => putData(k, value); null
  }
  // Reflection erases Key's type parameter, as in the project's user-data table.
  private def putData(key: Key[?], value: AnyRef): Unit = data.putUserData(key.asInstanceOf[Key[AnyRef]], value)
  private lazy val caret: Caret = tableProxy(classOf[Caret], "CompletionCaret") {
    case ("getEditor", _) => editor
    case ("getOffset", _) => Integer.valueOf(caretOffset)
    case ("getSelectionStart", _) => Integer.valueOf(selectionStart)
    case ("getSelectionEnd", _) => Integer.valueOf(selectionEnd)
    case ("hasSelection", _) => java.lang.Boolean.valueOf(selectionStart != selectionEnd)
    // SelectWordUtil checks whether the current selection still fits inside whitespace before expanding it.
    case ("getSelectionRange", _) => new TextRange(selectionStart, selectionEnd)
    case ("moveToOffset", Array(at: Integer)) => move(at.intValue); null
    case ("setSelection", Array(start: Integer, end: Integer)) => select(start.intValue, end.intValue); null
    case ("removeSelection", _) => removeSelection(); null
  }
  private lazy val model: CaretModel = tableProxy(classOf[CaretModel], "CompletionCaretModel") {
    case ("getCurrentCaret" | "getPrimaryCaret", _) => caret
    case ("getOffset", _) => Integer.valueOf(caretOffset)
    case ("getAllCarets", _) => java.util.List.of(caret)
    case ("moveToOffset", Array(at: Integer)) => move(at.intValue); null
  }
  private lazy val selection: SelectionModel = tableProxy(classOf[SelectionModel], "CompletionSelection") {
    case ("getSelectionStart", _) => Integer.valueOf(selectionStart)
    case ("getSelectionEnd", _) => Integer.valueOf(selectionEnd)
    case ("hasSelection", _) => java.lang.Boolean.valueOf(selectionStart != selectionEnd)
    case ("setSelection", Array(start: Integer, end: Integer)) => select(start.intValue, end.intValue); null
    case ("removeSelection", _) => removeSelection(); null
  }

  /** Detaches position tracking without changing the document or disposing its PSI. */
  override def close(): Unit = document.removeDocumentListener(listener)
}
