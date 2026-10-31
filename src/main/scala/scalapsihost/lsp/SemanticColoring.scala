package scalapsihost.lsp

import org.eclipse.lsp4j.{SemanticTokenModifiers, SemanticTokenTypes, SemanticTokensLegend}

import scalapsihost.language.{SemanticToken, SemanticTokenKind, SemanticTokenModifier}
import scalapsihost.LineColumn

import scala.jdk.CollectionConverters.*

/** The LSP semantic-token legend and relative integer encoding for workspace semantic categories. */
object SemanticColoring {
  private val types = Seq(
    SemanticTokenTypes.Namespace, SemanticTokenTypes.Type, SemanticTokenTypes.Class, SemanticTokenTypes.Enum,
    SemanticTokenTypes.Interface, SemanticTokenTypes.TypeParameter, SemanticTokenTypes.Parameter,
    SemanticTokenTypes.Variable, SemanticTokenTypes.Property, SemanticTokenTypes.EnumMember,
    SemanticTokenTypes.Function, SemanticTokenTypes.Method, SemanticTokenTypes.Keyword, SemanticTokenTypes.Decorator,
  )
  private val modifiers = Seq(SemanticTokenModifiers.Readonly, SemanticTokenModifiers.Abstract, SemanticTokenModifiers.Declaration)

  /** The token types and modifiers whose indexes are used by [[encode]]. */
  val legend: SemanticTokensLegend = new SemanticTokensLegend(types.asJava, modifiers.asJava)

  private val kindType: Map[SemanticTokenKind, String] = Map(
    SemanticTokenKind.Namespace -> SemanticTokenTypes.Namespace,
    SemanticTokenKind.Type -> SemanticTokenTypes.Type,
    SemanticTokenKind.Class -> SemanticTokenTypes.Class,
    SemanticTokenKind.Enum -> SemanticTokenTypes.Enum,
    SemanticTokenKind.Interface -> SemanticTokenTypes.Interface,
    SemanticTokenKind.TypeParameter -> SemanticTokenTypes.TypeParameter,
    SemanticTokenKind.Parameter -> SemanticTokenTypes.Parameter,
    SemanticTokenKind.Variable -> SemanticTokenTypes.Variable,
    SemanticTokenKind.Property -> SemanticTokenTypes.Property,
    SemanticTokenKind.EnumMember -> SemanticTokenTypes.EnumMember,
    SemanticTokenKind.Function -> SemanticTokenTypes.Function,
    SemanticTokenKind.Method -> SemanticTokenTypes.Method,
    SemanticTokenKind.Keyword -> SemanticTokenTypes.Keyword,
    SemanticTokenKind.Decorator -> SemanticTokenTypes.Decorator,
  )

  private val modifierName: Map[SemanticTokenModifier, String] = Map(
    SemanticTokenModifier.Readonly -> SemanticTokenModifiers.Readonly,
    SemanticTokenModifier.Abstract -> SemanticTokenModifiers.Abstract,
    SemanticTokenModifier.Declaration -> SemanticTokenModifiers.Declaration,
  )

  /**
   * Encodes workspace tokens for LSP. Tokens may not overlap: among equal ranges the last wins, and an overlapping
   * later range is dropped.
   */
  def encode(tokens: Seq[SemanticToken]): Seq[Int] = {
    val lastPerRange = tokens.groupMapReduce(token => (token.start, token.length))(identity)((_, later) => later).values
    val ordered = lastPerRange.toSeq.sortBy(token => (token.start.line, token.start.column))
    val kept = ordered.foldLeft(Vector.empty[SemanticToken]) { (acc, next) =>
      acc.lastOption match {
        case Some(last) if last.start.line == next.start.line && next.start.column < last.start.column + last.length => acc
        case _ => acc :+ next
      }
    }
    kept.zipWithIndex.flatMap { (token, i) =>
      val previous = if (i == 0) LineColumn(0, 0) else kept(i - 1).start
      val deltaLine = token.start.line - previous.line
      val deltaColumn = if (deltaLine == 0) token.start.column - previous.column else token.start.column
      val modifierBits = token.modifiers.map(m => 1 << modifiers.indexOf(modifierName(m))).sum
      Seq(deltaLine, deltaColumn, token.length, types.indexOf(kindType(token.kind)), modifierBits)
    }
  }
}
