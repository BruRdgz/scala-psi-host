package scalapsihost.lsp

import com.intellij.openapi.editor.colors.TextAttributesKey
import org.eclipse.lsp4j.{SemanticTokenModifiers, SemanticTokenTypes, SemanticTokensLegend}
import org.jetbrains.plugins.scala.highlighter.DefaultHighlighter as H
import scalapsihost.LineColumn

import scala.jdk.CollectionConverters.*

/**
 * The plugin's highlighting keys as LSP semantic tokens. Keys with no counterpart in LSP's vocabulary (collections,
 * `Predef`, implicit conversions, ScalaDoc markup) are not sent: the editor's own grammar colors the rest.
 */
object SemanticColoring {
  private val types = Seq(
    SemanticTokenTypes.Namespace, SemanticTokenTypes.Type, SemanticTokenTypes.Class, SemanticTokenTypes.Enum,
    SemanticTokenTypes.Interface, SemanticTokenTypes.TypeParameter, SemanticTokenTypes.Parameter,
    SemanticTokenTypes.Variable, SemanticTokenTypes.Property, SemanticTokenTypes.EnumMember,
    SemanticTokenTypes.Function, SemanticTokenTypes.Method, SemanticTokenTypes.Keyword, SemanticTokenTypes.Decorator,
  )
  private val modifiers = Seq(SemanticTokenModifiers.Readonly, SemanticTokenModifiers.Abstract, SemanticTokenModifiers.Declaration)

  val legend: SemanticTokensLegend = new SemanticTokensLegend(types.asJava, modifiers.asJava)

  private val byKey: Map[TextAttributesKey, (String, Set[String])] = {
    import SemanticTokenModifiers.{Abstract, Declaration, Readonly}
    import SemanticTokenTypes.*
    Map(
      H.KEYWORD -> (Keyword, Set()),
      H.PACKAGE -> (Namespace, Set()),
      H.CLASS -> (Class, Set()),
      H.ABSTRACT_CLASS -> (Class, Set(Abstract)),
      H.OBJECT -> (Class, Set(Readonly)),
      H.TRAIT -> (Interface, Set()),
      H.ENUM -> (Enum, Set()),
      H.ENUM_SINGLETON_CASE -> (EnumMember, Set()),
      H.ENUM_CLASS_CASE -> (EnumMember, Set()),
      H.TYPEPARAM -> (TypeParameter, Set()),
      H.TYPE_ALIAS -> (Type, Set()),
      H.LOCAL_VALUES -> (Variable, Set(Readonly)),
      H.LOCAL_LAZY -> (Variable, Set(Readonly)),
      H.VALUES -> (Variable, Set(Readonly)),
      H.LAZY -> (Variable, Set(Readonly)),
      H.PATTERN -> (Variable, Set(Readonly)),
      H.GENERATOR -> (Variable, Set(Readonly)),
      H.GIVEN -> (Variable, Set(Readonly)),
      H.LOCAL_VARIABLES -> (Variable, Set()),
      H.VARIABLES -> (Variable, Set()),
      H.CASE_CLASS_FIELD -> (Property, Set(Readonly)),
      H.CASE_CLASS_VAR_FIELD -> (Property, Set()),
      H.PARAMETER -> (Parameter, Set()),
      H.PARAMETER_OF_ANONIMOUS_FUNCTION -> (Parameter, Set()),
      H.NAMED_ARGUMENT -> (Parameter, Set()),
      H.METHOD_CALL -> (Method, Set()),
      H.OBJECT_METHOD_CALL -> (Method, Set()),
      H.LOCAL_METHOD_CALL -> (Function, Set()),
      H.METHOD_DECLARATION -> (Method, Set(Declaration)),
      H.ANNOTATION -> (Decorator, Set()),
      H.ANNOTATION_ATTRIBUTE -> (Property, Set()),
    )
  }

  /** A colored range of one line. */
  final case class Token(start: LineColumn, length: Int, key: TextAttributesKey)

  /**
   * The tokens in LSP's relative encoding. Tokens may not overlap: of tokens on the same range the last wins (the
   * annotator's, which comes after the visitor's); a token that overlaps an earlier, different range is dropped.
   */
  def encode(tokens: Seq[Token]): Seq[Int] = {
    val mapped = tokens.flatMap(t => byKey.get(t.key).map(t -> _))
    val lastPerRange = mapped.groupMapReduce(m => (m._1.start, m._1.length))(identity)((_, later) => later).values
    val ordered = lastPerRange.toSeq.sortBy(m => (m._1.start.line, m._1.start.column))
    val kept = ordered.foldLeft(Vector.empty[(Token, (String, Set[String]))]) { (acc, next) =>
      acc.lastOption match {
        case Some((last, _)) if last.start.line == next._1.start.line && next._1.start.column < last.start.column + last.length => acc
        case _ => acc :+ next
      }
    }
    kept.zipWithIndex.flatMap { case ((token, (tokenType, tokenModifiers)), i) =>
      val previous = if (i == 0) LineColumn(0, 0) else kept(i - 1)._1.start
      val deltaLine = token.start.line - previous.line
      val deltaColumn = if (deltaLine == 0) token.start.column - previous.column else token.start.column
      val modifierBits = tokenModifiers.map(m => 1 << modifiers.indexOf(m)).sum
      Seq(deltaLine, deltaColumn, token.length, types.indexOf(tokenType), modifierBits)
    }
  }
}
