package scalapsihost;

import com.intellij.codeHighlighting.TextEditorHighlightingPass;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import org.jetbrains.plugins.scala.annotator.hints.Text;
import org.jetbrains.plugins.scala.codeInsight.hints.ScalaHintsSettings;
import org.jetbrains.plugins.scala.codeInsight.hints.methodChains.AlignedHintTemplate;
import org.jetbrains.plugins.scala.codeInsight.implicits.ImplicitHintsPass;
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile;
import scala.Tuple2;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

import java.util.List;

/** The pass and template accessors are public in bytecode, but their Scala package visibility prevents Scala callers. */
public final class HostImplicitHintsPass {
  public record ChainHint(int offset, Seq<Text> parts) {}
  public record MethodChain(List<ChainHint> hints, PsiElement outer) {}

  public static TextEditorHighlightingPass create(Editor editor, ScalaFile file, ScalaHintsSettings settings) {
    return new ImplicitHintsPass(editor, file, settings, false);
  }

  public static List<MethodChain> methodChains(TextEditorHighlightingPass pass) {
    if (!(pass instanceof ImplicitHintsPass hintsPass)) {
      throw new IllegalArgumentException("not the plugin's hints pass: " + pass);
    }
    // The public accessor's generic signature is erased because its template type is Scala-private.
    Seq<?> groups = hintsPass.org$jetbrains$plugins$scala$codeInsight$hints$methodChains$ScalaMethodChainInlayHintsPass$$collectedHintTemplates();
    return CollectionConverters.asJava(groups).stream().map(HostImplicitHintsPass::methodChain).toList();
  }

  private static MethodChain methodChain(Object value) {
    if (!(value instanceof Tuple2<?, ?> group) || !(group._1() instanceof Seq<?> templates) ||
        !(group._2() instanceof PsiElement outer)) {
      throw new IllegalStateException("not the plugin's method-chain group: " + value);
    }
    List<ChainHint> hints = CollectionConverters.asJava(templates).stream().map(HostImplicitHintsPass::chainHint).toList();
    return new MethodChain(hints, outer);
  }

  private static ChainHint chainHint(Object value) {
    if (!(value instanceof AlignedHintTemplate hint)) {
      throw new IllegalStateException("not the plugin's method-chain template: " + value);
    }
    return new ChainHint(hint.endOffset(), hint.textParts());
  }
}
