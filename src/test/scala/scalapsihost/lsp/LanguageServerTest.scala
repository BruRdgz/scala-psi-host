package scalapsihost.lsp

import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.LanguageServer
import scalapsihost.TestHost

import java.io.{PipedInputStream, PipedOutputStream}
import java.nio.file.{Files, Path}
import java.util.concurrent.{CompletableFuture, LinkedBlockingQueue, TimeUnit}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/**
 * The language server end to end: a client and the server connected through JSON-RPC streams, on the BSP fixture
 * (`fixtures/bsp-project`, a Scala 2.13 build: errors come from the plugin). Runs in a JVM of its own (see
 * `Test / testGrouping`): it opens the fixture's modules in the host.
 */
class LanguageServerTest extends munit.FunSuite {
  override val munitTimeout: Duration = 10.minutes

  private val fixture = Path.of("fixtures/bsp-project").toAbsolutePath
  private val main = fixture.resolve("app/src/main/scala/app/Main.scala")
  private val greeting = fixture.resolve("core/src/main/scala/core/Greeting.scala")
  private val counter = fixture.resolve("core/src/main/scala/core/Counter.scala")

  /** Records what the server publishes. */
  private object Client extends ClientBase {
    val published = new LinkedBlockingQueue[PublishDiagnosticsParams]()
    override def publishDiagnostics(params: PublishDiagnosticsParams): Unit = published.add(params)
    override def telemetryEvent(o: AnyRef): Unit = ()
    override def showMessage(params: MessageParams): Unit = ()
    override def showMessageRequest(params: ShowMessageRequestParams): CompletableFuture[MessageActionItem] =
      CompletableFuture.completedFuture(null)
    override def logMessage(params: MessageParams): Unit = ()
  }

  private lazy val server: LanguageServer = {
    val toServer = new PipedOutputStream
    val toClient = new PipedOutputStream
    val implementation = new ScalaLanguageServer(TestHost.host)
    val serverSide = LSPLauncher.createServerLauncher(implementation, new PipedInputStream(toServer), toClient)
    implementation.connect(serverSide.getRemoteProxy)
    serverSide.startListening()
    val clientSide = LSPLauncher.createClientLauncher(Client, new PipedInputStream(toClient), toServer)
    clientSide.startListening()
    clientSide.getRemoteProxy
  }

  private def diagnosticsOf(path: Path, version: Int): Seq[Diagnostic] = {
    val deadline = System.nanoTime() + 5.minutes.toNanos
    var found: Option[PublishDiagnosticsParams] = None
    while (found.isEmpty) {
      val next = Client.published.poll(deadline - System.nanoTime(), TimeUnit.NANOSECONDS)
      if (next == null) fail(s"no diagnostics for $path version $version")
      if (next.getUri == path.toUri.toString && next.getVersion == version) found = Some(next)
    }
    found.get.getDiagnostics.asScala.toSeq
  }

  private def errors(diagnostics: Seq[Diagnostic]) =
    diagnostics.filter(_.getSeverity == DiagnosticSeverity.Error).map(_.getMessage)

  private def document(path: Path) = new TextDocumentIdentifier(path.toUri.toString)

  test("initializes on the workspace's build and announces its features") {
    val params = new InitializeParams
    params.setWorkspaceFolders(java.util.List.of(new WorkspaceFolder(fixture.toUri.toString, "bsp-project")))
    val capabilities = server.initialize(params).get(1, TimeUnit.MINUTES).getCapabilities
    assertEquals(capabilities.getTextDocumentSync.getLeft, TextDocumentSyncKind.Incremental)
    assert(capabilities.getHoverProvider.getLeft)
    assert(capabilities.getDefinitionProvider.getLeft)
    assert(capabilities.getTypeDefinitionProvider.getLeft)
    assert(capabilities.getDocumentHighlightProvider.getLeft)
    assert(capabilities.getDocumentSymbolProvider.getLeft)
    assert(capabilities.getFoldingRangeProvider.getLeft)
    assert(capabilities.getSemanticTokensProvider.getFull.getLeft)
    server.initialized(new InitializedParams)
  }

  test("publishes a document's problems, and new ones after a change") {
    server.getTextDocumentService.didOpen(new DidOpenTextDocumentParams(
      new TextDocumentItem(main.toUri.toString, "scala", 1, Files.readString(main))))
    assertEquals(errors(diagnosticsOf(main, 1)), Seq.empty)

    // `val text: String = ...` becomes `val text: Int = ...` (line 6, columns 14-20).
    val change = new TextDocumentContentChangeEvent(new Range(new Position(6, 14), new Position(6, 20)), "Int")
    server.getTextDocumentService.didChange(new DidChangeTextDocumentParams(
      new VersionedTextDocumentIdentifier(main.toUri.toString, 2), java.util.List.of(change)))
    assertEquals(errors(diagnosticsOf(main, 2)), Seq("Type mismatch. Required: Int, found: String"))
  }

  // After the change above (`String` became `Int`), `.text` is at line 6, column 50: Greeting.text, declared on line 3
  // of Greeting.scala.
  test("shows the signature of the symbol under the cursor") {
    val hover = server.getTextDocumentService.hover(new HoverParams(document(main), new Position(6, 50))).get(1, TimeUnit.MINUTES)
    assert(hover.getContents.getRight.getValue.contains("def text: String"), hover.getContents.getRight.getValue)
  }

  // The cursor on a declaration's own name (`Greeting` on line 2 of Greeting.scala).
  test("shows the signature of the declaration under the cursor") {
    val hover = server.getTextDocumentService.hover(new HoverParams(document(greeting), new Position(2, 20))).get(1, TimeUnit.MINUTES)
    assert(hover != null, "no hover on a declaration's name")
    assert(hover.getContents.getRight.getValue.contains("case class Greeting"), hover.getContents.getRight.getValue)
  }

  // Inside the literal "Hello, $name": text, not a symbol.
  test("shows nothing on text that is not a symbol") {
    val hover = server.getTextDocumentService.hover(new HoverParams(document(greeting), new Position(3, 24))).get(1, TimeUnit.MINUTES)
    assertEquals(hover, null)
  }

  test("goes to the declaration of the symbol under the cursor") {
    val locations = server.getTextDocumentService.definition(new DefinitionParams(document(main), new Position(6, 50)))
      .get(1, TimeUnit.MINUTES).getLeft.asScala.toSeq
    assertEquals(locations.map(l => (l.getUri, l.getRange.getStart)), Seq((greeting.toUri.toString, new Position(3, 6))))
  }

  // `println` (line 7 of Main.scala): Predef's, in scala-library's source jar, which the build provides.
  test("goes to a library declaration in the library's sources") {
    val locations = server.getTextDocumentService.definition(new DefinitionParams(document(main), new Position(7, 4)))
      .get(1, TimeUnit.MINUTES).getLeft.asScala.toSeq
    assertEquals(locations.size, 1, locations)
    val target = Path.of(java.net.URI.create(locations.head.getUri))
    assert(target.toString.endsWith("scala/Predef.scala"), target)
    val line = Files.readAllLines(target).get(locations.head.getRange.getStart.getLine)
    assert(line.substring(locations.head.getRange.getStart.getCharacter).startsWith("println"), line)
    // The editor may open it; it is not a project source, so the server answers nothing about it.
    val hover = server.getTextDocumentService.hover(new HoverParams(new TextDocumentIdentifier(locations.head.getUri), new Position(0, 0)))
      .get(1, TimeUnit.MINUTES)
    assertEquals(hover, null)
  }

  // `zero` (line 20) returns a Counter, declared on line 8.
  test("goes to the declaration of a symbol's type") {
    val locations = server.getTextDocumentService.typeDefinition(new TypeDefinitionParams(document(counter), new Position(20, 6)))
      .get(1, TimeUnit.MINUTES).getLeft.asScala.toSeq
    assertEquals(locations.map(l => (l.getUri, l.getRange.getStart)), Seq((counter.toUri.toString, new Position(8, 6))))
  }

  // `count`, declared on line 9: assigned on lines 12 and 13, read on lines 13 and 16.
  test("highlights a symbol's occurrences in the file, reads and writes") {
    val highlights = server.getTextDocumentService.documentHighlight(new DocumentHighlightParams(document(counter), new Position(9, 14)))
      .get(1, TimeUnit.MINUTES).asScala.toSeq
    val found = highlights.map(h => (h.getRange.getStart.getLine, h.getRange.getStart.getCharacter, h.getKind)).sortBy(h => (h._1, h._2))
    assertEquals(found, Seq(
      (9, 14, DocumentHighlightKind.Write),
      (12, 4, DocumentHighlightKind.Write),
      (13, 4, DocumentHighlightKind.Write),
      (13, 12, DocumentHighlightKind.Read),
      (16, 21, DocumentHighlightKind.Read),
    ))
  }

  test("outlines a file as the structure view shows it") {
    val symbols = server.getTextDocumentService.documentSymbol(new DocumentSymbolParams(document(counter)))
      .get(1, TimeUnit.MINUTES).asScala.toSeq.map(_.getRight)
    assertEquals(symbols.map(s => (s.getName, s.getKind)), Seq(("Counter", SymbolKind.Class), ("Counter", SymbolKind.Module)))
    val members = symbols.head.getChildren.asScala.map(s => s.getName -> s.getDetail).toMap
    // As the IDE shows it: parameter types only.
    assertEquals(members.get("increment"), Some("(Int): Unit"))
    assert(members.contains("count") && members.contains("current"), members)
    // The class's range is its whole definition, doc comment included (line 5), as LSP asks; its name's range, the name.
    assertEquals(symbols.head.getRange.getStart, new Position(5, 0))
    assertEquals(symbols.head.getSelectionRange, new Range(new Position(8, 6), new Position(8, 13)))
  }

  // Line 13, `    count = count + 1`: the assigned and the read variable; line 20, `  def zero: Counter = new Counter(0)`.
  test("colors identifiers by what they declare or refer to") {
    val data = server.getTextDocumentService.semanticTokensFull(new SemanticTokensParams(document(counter)))
      .get(1, TimeUnit.MINUTES).getData.asScala.map(_.toInt).toSeq
    val legend = SemanticColoring.legend
    // Decoded: (line, column) -> (type, modifiers).
    val tokens = data.grouped(5).foldLeft((Map.empty[(Int, Int), (String, Set[String])], 0, 0)) {
      case ((found, line, column), Seq(deltaLine, deltaColumn, _, tokenType, bits)) =>
        val l = line + deltaLine
        val c = if (deltaLine == 0) column + deltaColumn else deltaColumn
        val modifiers = legend.getTokenModifiers.asScala.zipWithIndex.collect { case (m, i) if (bits & (1 << i)) != 0 => m }.toSet
        (found + ((l, c) -> (legend.getTokenTypes.get(tokenType), modifiers)), l, c)
      case (state, _) => state
    }._1
    assertEquals(tokens.get((13, 4)).map(_._1), Some(SemanticTokenTypes.Variable))
    assertEquals(tokens.get((13, 12)).map(_._1), Some(SemanticTokenTypes.Variable))
    assertEquals(tokens.get((20, 6)), Some(SemanticTokenTypes.Method -> Set(SemanticTokenModifiers.Declaration)))
    assertEquals(tokens.get((20, 12)).map(_._1), Some(SemanticTokenTypes.Class))
  }

  test("folds imports, doc comments and bodies, leaving closing braces visible") {
    val folds = server.getTextDocumentService.foldingRange(new FoldingRangeRequestParams(document(counter)))
      .get(1, TimeUnit.MINUTES).asScala.toSeq
    val found = folds.map(f => (f.getStartLine.toInt, f.getEndLine.toInt, Option(f.getKind))).sortBy(_._1)
    assertEquals(found, Seq(
      (2, 3, Some(FoldingRangeKind.Imports)),
      (5, 7, Some(FoldingRangeKind.Comment)),
      (8, 16, None),
      (11, 13, None),
      (19, 20, None),
    ))
  }
}
