package scalapsihost.language

import com.intellij.codeHighlighting.TextEditorHighlightingPass
import com.intellij.openapi.progress.ProgressManager
import org.jetbrains.plugins.scala.annotator.hints.{AnnotatorHints, Hint}
import org.jetbrains.plugins.scala.codeInsight.hints.ScalaHintsSettings
import org.jetbrains.plugins.scala.codeInsight.implicits.{ImplicitHints, TextPartsHintRenderer}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

import scalapsihost.platform.{HostImplicitHintsPass, HostLock, RequestEditor}

import scala.collection.mutable.Buffer
import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * The inline hints the IDE shows in a Scala file: types of definitions without one, parameter names, implicit and
 * `using` arguments, conversions, method chains and type arguments. They come from the plugin's own hints pass, the
 * one the IDE's daemon runs, read as the hints it collected before an editor would draw them.
 */
object InlayHints {

  /** A hint at `offset`, between two characters of the file. */
  final case class InlayHint(offset: Int, text: String)

  /**
   * The IDE's settings, but for range hints: RangeInlayHintsPass casts its editor to EditorImpl to build drawn
   * presentations, which have no text. Lazy: the adapter reads the plugin's settings services, registered after
   * the host first enables implicit hints.
   */
  private lazy val Settings = new ScalaHintsSettings.CodeInsightSettingsAdapter {
    override def showRangeHintsForToAndUntil: Boolean = false
    override def showExclusiveRangeHint: Boolean = false
  }

  private val PassClass = Class.forName("org.jetbrains.plugins.scala.codeInsight.implicits.ImplicitHintsPass")

  // The pass exposes collection but keeps its results private, with no accessor before editor rendering.
  private val CollectedHints = {
    val field = PassClass.getDeclaredField("hints")
    field.setAccessible(true)
    field
  }

  // The public setter rebinds IDE shortcuts and fails without KeymapManager. A field write is smaller than
  // adding the absent keymap API and adapters solely for those shortcuts; the host only needs the enabled flag.
  private val Enabled = ImplicitHints.getClass.getDeclaredField("_enabled")
  Enabled.setAccessible(true)

  /** Sets the plugin's implicit-hints flag without registering the IDE keymap services it otherwise requires. */
  private[scalapsihost] def setImplicitHintsEnabled(enabled: Boolean): Unit = Enabled.setBoolean(ImplicitHints, enabled)

  /** The hints in document order, preserving collection order when several share an offset. */
  def in(file: ScalaFile): Seq[InlayHint] = HostLock.query {
    Using.resource(new RequestEditor(file.getViewProvider.getDocument, file.getProject, 0)) { editor =>
      val pass = HostImplicitHintsPass.create(editor.editor, file, Settings)
      pass.doCollectInformation(ProgressManager.getInstance.getProgressIndicator)
      val ordinary = collected(pass).map(hint => InlayHint(hint.position.getOffset(hint.element) + hint.offsetDelta, text(hint)))
      (ordinary ++ methodChains(pass)).sortBy(_.offset)
    }
  }

  /** Renders the hint, including nested parts when the IDE's implicit-hint expansion toggle is on. */
  private def text(hint: Hint): String =
    if (ImplicitHints.expanded) {
      val renderer = new TextPartsHintRenderer(hint.parts, hint.menu, hint.corners)
      renderer.expand()
      renderer.parts.map(_.string).mkString
    } else hint.parts.map(_.string).mkString

  /** Converts the plugin's chained-call hints and omits layout-only or mismatch-hidden entries. */
  private def methodChains(pass: TextEditorHighlightingPass): Seq[InlayHint] =
    HostImplicitHintsPass.methodChains(pass).asScala.toSeq.flatMap { group =>
      val hints = group.hints.asScala.toSeq
      // As regenerateMethodChainHints does, omit the last hint after an annotated type mismatch.
      val shown = if (AnnotatorHints.in(group.outer).nonEmpty) hints.dropRight(1) else hints
      if (shown.size < 2) Seq.empty
      // Blank templates only pad the IDE's vertical alignment; they have no label to show in LSP.
      else shown.filterNot(_.parts.forall(_.string.isBlank)).map(hint => InlayHint(hint.offset, hint.parts.map(_.string).mkString))
    }

  /** Reads the plugin pass's private result buffer, rejecting an unexpected runtime shape. */
  private def collected(pass: TextEditorHighlightingPass): Seq[Hint] = CollectedHints.get(pass) match {
    case hints: Buffer[?] => hints.toSeq.map {
      case hint: Hint => hint
      case other => throw new IllegalStateException(s"not a plugin hint: $other")
    }
    case other => throw new IllegalStateException(s"not the plugin's hints buffer: $other")
  }
}
