package scalapsihost.language

import scalapsihost.LineColumn

import java.nio.file.Path

/** Diagnostic severity without a dependency on either the IDE or LSP severity enums. */
enum ProblemSeverity { case Error, Warning, Information }

/** Symbol categories returned by workspace navigation operations. */
enum WorkspaceSymbolKind {
  case EnumMember, Enum, Interface, Module, Class, Constructor, Function, Method, Property, Namespace, Variable, Constant, Object
}

/** A problem in a file, from the Scala plugin or the build compiler. */
final case class FileProblem(
  severity: ProblemSeverity,
  start: LineColumn,
  end: LineColumn,
  message: String,
  source: String,
)

/** An occurrence of a symbol and whether it writes the symbol. */
final case class Occurrence(range: (LineColumn, LineColumn), write: Boolean)

/** A range in a project source file. */
final case class SourceSpan(path: Path, range: (LineColumn, LineColumn))

/** A project symbol found by name, with its containing package or class and source range. */
final case class WorkspaceSymbolMatch(name: String, kind: WorkspaceSymbolKind, container: Option[String], location: SourceSpan)

/** The text between `start` and `end` becomes `text`. */
final case class TextChange(start: LineColumn, end: LineColumn, text: String)

/** The changes to a project file. */
final case class FileChanges(path: Path, changes: Seq[TextChange])

/** A previewed editor action, with its protocol-neutral outcome and applicable changes. */
final case class CodeActionResult(title: String, kind: String, disabled: Option[String])

/** A completion candidate with only the values needed by an editor. */
final case class CompletionCandidate(
  label: String,
  tail: Option[String],
  typeText: Option[String],
  kind: Completion.Kind,
  deprecated: Boolean,
  lookup: String,
  prefixText: String,
  hasHandler: Boolean,
  identity: String,
)

/** A completion suggestion and the range its lookup text replaces. */
final case class CompletionChoice(suggestion: CompletionCandidate, start: LineColumn, end: LineColumn)

/** What a chosen suggestion's insert handler does after its lookup text is inserted. */
final case class CompletionFollowUp(changes: Seq[TextChange], caret: LineColumn)

/** An inline hint at a document position. */
final case class InlineHint(at: LineColumn, text: String)

/** The best-first completion choices and whether the client can ask for more. */
final case class CompletionChoices(items: Seq[CompletionChoice], incomplete: Boolean)

/** A range in rendered signature text. */
final case class OffsetRange(start: Int, end: Int)

/** One rendered call signature, including its current parameter when available. */
final case class SignatureInfo(text: String, current: Option[OffsetRange], applicable: Boolean, deprecated: Boolean)

/** Rendered overloads and the active overload index. */
final case class SignatureChoices(items: Seq[SignatureInfo], active: Option[Int])

/** A declaration in a file outline, including its ranges and nested declarations. */
final case class OutlineSymbol(
  name: String,
  detail: String,
  kind: WorkspaceSymbolKind,
  range: (LineColumn, LineColumn),
  nameRange: (LineColumn, LineColumn),
  children: Seq[OutlineSymbol],
)

/** A folding region category and its hidden line range. */
final case class Fold(startLine: Int, endLine: Int, kind: Fold.Kind)

object Fold {
  enum Kind { case Code, Comment, Imports, Region }
  export Kind.*
}

/** A semantic category understood by the workspace layer, independent of the LSP token-number encoding. */
enum SemanticTokenKind {
  case Namespace, Type, Class, Enum, Interface, TypeParameter, Parameter, Variable, Property, EnumMember,
    Function, Method, Keyword, Decorator
}

enum SemanticTokenModifier { case Readonly, Abstract, Declaration }

/** One single-line semantic token. */
final case class SemanticToken(
  start: LineColumn,
  length: Int,
  kind: SemanticTokenKind,
  modifiers: Set[SemanticTokenModifier],
)
