package scalapsihost.lsp

import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.launch.LSPLauncher
import scalapsihost.{BspImport, TestHost}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PipedInputStream, PipedOutputStream}
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

  /** Records what the server sends. */
  private final class RecordingClient extends ClientBase {
    val published = new LinkedBlockingQueue[PublishDiagnosticsParams]()
    val states = new LinkedBlockingQueue[HostProtocol.ServerState]()
    val logged = new LinkedBlockingQueue[MessageParams]()
    val shown = new LinkedBlockingQueue[MessageParams]()
    override def publishDiagnostics(params: PublishDiagnosticsParams): Unit = published.add(params)
    override def state(state: HostProtocol.ServerState): Unit = states.add(state)
    override def telemetryEvent(o: AnyRef): Unit = ()
    override def showMessage(params: MessageParams): Unit = shown.add(params)
    override def showMessageRequest(params: ShowMessageRequestParams): CompletableFuture[MessageActionItem] =
      CompletableFuture.completedFuture(null)
    override def logMessage(params: MessageParams): Unit = logged.add(params)
  }

  /** A server for `client`, connected to it as an editor connects (the server's own messages included). */
  private def connect(client: RecordingClient): HostProtocol.LanguageServerWithStatus = {
    val toServer = new PipedOutputStream
    val toClient = new PipedOutputStream
    val implementation = new ScalaLanguageServer(TestHost.host)
    val serverSide = new LSPLauncher.Builder[HostProtocol.Client]().setLocalService(implementation)
      .setRemoteInterface(classOf[HostProtocol.Client]).setInput(new PipedInputStream(toServer)).setOutput(toClient).create()
    implementation.connect(serverSide.getRemoteProxy)
    serverSide.startListening()
    val clientSide = new LSPLauncher.Builder[HostProtocol.LanguageServerWithStatus]().setLocalService(client)
      .setRemoteInterface(classOf[HostProtocol.LanguageServerWithStatus]).setInput(new PipedInputStream(toClient)).setOutput(toServer).create()
    clientSide.startListening()
    clientSide.getRemoteProxy
  }

  private val Client = new RecordingClient
  private lazy val server = connect(Client)
  private var ownsFixtureServer = false

  override def afterAll(): Unit = {
    server.shutdown().get(5, TimeUnit.SECONDS)
    server.exit()
    if (ownsFixtureServer)
      assert(!Files.exists(fixture.resolve("project/target/active.json")), "the language server left its sbt server running")
  }

  private def initialize(server: HostProtocol.LanguageServerWithStatus, root: Path): InitializeResult = {
    val params = new InitializeParams
    params.setWorkspaceFolders(java.util.List.of(new WorkspaceFolder(root.toUri.toString, root.getFileName.toString)))
    server.initialize(params).get(1, TimeUnit.MINUTES)
  }

  /** The states the server reports until it reaches `last`. */
  private def statesUntil(client: RecordingClient, last: String): Seq[String] = {
    val seen = Seq.newBuilder[String]
    var state = ""
    while (state != last) {
      val next = client.states.poll(5, TimeUnit.MINUTES)
      if (next == null) fail(s"the server never reported '$last' (reported ${seen.result()})")
      state = next.state
      seen += state
    }
    seen.result()
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

  test("ends once when the client's input ends, with the shutdown status") {
    for (shutdown <- Seq(false, true)) {
      val exited = new LinkedBlockingQueue[Int]()
      val implementation = new ScalaLanguageServer(TestHost.host, code => { exited.add(code); () })
      if (shutdown) implementation.shutdown().get(10, TimeUnit.SECONDS)
      Main.listen(implementation, new ByteArrayInputStream(Array.emptyByteArray), new ByteArrayOutputStream)
      assertEquals(Option(exited.poll(10, TimeUnit.SECONDS)), Some(if (shutdown) 0 else 1))
      implementation.exit()
      assert(exited.isEmpty, "exit was called more than once")
    }
  }

  test("ends when the watched client process exits without an exit notification") {
    val exited = new LinkedBlockingQueue[Int]()
    val implementation = new ScalaLanguageServer(TestHost.host, code => { exited.add(code); () })
    implementation.connect(new RecordingClient)
    val root = Files.createTempDirectory("scala-psi-host-client-exit")
    val process = new ProcessBuilder("sleep", "1").start()
    try {
      val params = new InitializeParams
      params.setRootUri(root.toUri.toString)
      params.setProcessId(Math.toIntExact(process.pid()))
      implementation.initialize(params).get(10, TimeUnit.SECONDS)
      assert(process.waitFor(10, TimeUnit.SECONDS), "the client process did not exit")
      assertEquals(Option(exited.poll(10, TimeUnit.SECONDS)), Some(1))
      implementation.exit()
      assert(exited.isEmpty, "exit was called more than once")
    } finally {
      process.destroyForcibly()
      implementation.exit()
      Files.delete(root)
    }
  }

  test("initializes on the workspace's build and announces its features") {
    ownsFixtureServer = BspImport.sbtServerState(fixture) match {
      case BspImport.SbtServerState.Missing | BspImport.SbtServerState.Inactive(_) => true
      case _ => false
    }
    val capabilities = initialize(server, fixture).getCapabilities
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

  test("reports its state, logs what it does and answers its status") {
    assertEquals(statesUntil(Client, "ready"), Seq("importing", "ready"))
    val logged = Client.logged.asScala.toSeq.map(_.getMessage)
    assert(logged.exists(_.contains("ready in")), logged.mkString("\n"))
    assert(logged.exists(_.contains("build imported in")), logged.mkString("\n"))
    val status = server.status().get(1, TimeUnit.MINUTES).asScala
    assertEquals(status("state"), "ready")
    // The fixture's four projects (core, app, core3 and the root), main and test targets each; Gson reads numbers
    // as doubles.
    assertEquals(status("project").asInstanceOf[java.util.Map[String, AnyRef]].get("modules"), java.lang.Double.valueOf(8))
    val requests = status("requests").asInstanceOf[java.util.Map[String, AnyRef]]
    assert(requests.containsKey("didChange") && requests.containsKey("problems"), requests)
  }

  test("tells the user, and its status, when the project cannot be opened") {
    val noBuild = Files.createTempDirectory("scala-psi-host-no-build")
    val client = new RecordingClient
    val other = connect(client)
    initialize(other, noBuild)
    assertEquals(statesUntil(client, "failed"), Seq("importing", "failed"))
    val shown = client.shown.poll(1, TimeUnit.MINUTES)
    assertEquals(shown.getType, MessageType.Error)
    assert(shown.getMessage.contains("could not open the project"), shown.getMessage)
    assertEquals(other.status().get(1, TimeUnit.MINUTES).get("state"), "failed")
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

  private def filesChanged(changes: (Path, FileChangeType)*): Unit = {
    server.getWorkspaceService.didChangeWatchedFiles(new DidChangeWatchedFilesParams(
      changes.map((path, kind) => new FileEvent(path.toUri.toString, kind)).asJava))
  }

  test("opens a source file created after the import, and lets it go when it is deleted") {
    val added = fixture.resolve("core/src/main/scala/core/Added.scala")
    Files.writeString(added, "package core\n\nobject Added {\n  val n: Int = Counter.zero.current\n}\n")
    try {
      filesChanged(added -> FileChangeType.Created)
      server.getTextDocumentService.didOpen(new DidOpenTextDocumentParams(
        new TextDocumentItem(added.toUri.toString, "scala", 1, Files.readString(added))))
      assertEquals(errors(diagnosticsOf(added, 1)), Seq.empty)
      // `zero` (line 3) is Counter's, on line 20 of Counter.scala.
      val locations = server.getTextDocumentService.definition(new DefinitionParams(document(added), new Position(3, 23)))
        .get(1, TimeUnit.MINUTES).getLeft.asScala.toSeq
      assertEquals(locations.map(l => (l.getUri, l.getRange.getStart)), Seq((counter.toUri.toString, new Position(20, 6))))
    } finally Files.delete(added)
    filesChanged(added -> FileChangeType.Deleted)
    // No longer a source: its problems are cleared, and nothing is answered about it.
    assertEquals(diagnosticsOf(added, 1), Seq.empty)
    assertEquals(server.getTextDocumentService.hover(new HoverParams(document(added), new Position(3, 23))).get(1, TimeUnit.MINUTES), null)
  }

  // Main.scala still has the unsaved change of the diagnostics test (`String` became `Int`). Without `-Xsource:3`,
  // its `import core.*` is no wildcard import: the module's new options must reach the plugin.
  test("imports the build again after a change to its definition, keeping unsaved changes") {
    val buildFile = fixture.resolve("build.sbt")
    val original = Files.readString(buildFile)
    val typeMismatch = Seq("Type mismatch. Required: Int, found: String")
    // A change that leaves the text as it is, under a new version: the server handles it after the build change
    // (documents are handled in order), so its problems are those of the imported build.
    def problemsAfterImport(version: Int) = {
      val unchanged = new TextDocumentContentChangeEvent(new Range(new Position(6, 14), new Position(6, 17)), "Int")
      server.getTextDocumentService.didChange(new DidChangeTextDocumentParams(
        new VersionedTextDocumentIdentifier(main.toUri.toString, version), java.util.List.of(unchanged)))
      errors(diagnosticsOf(main, version))
    }
    try {
      Files.writeString(buildFile, original.replace("""scalacOptions += "-Xsource:3"""", """scalacOptions += "-deprecation""""))
      filesChanged(buildFile -> FileChangeType.Changed)
      val withoutSource3 = problemsAfterImport(3)
      assert(withoutSource3.nonEmpty && withoutSource3 != typeMismatch, withoutSource3)
    } finally {
      Files.writeString(buildFile, original)
      filesChanged(buildFile -> FileChangeType.Changed)
    }
    // Back to the original options; the editor's text was kept through both imports.
    assertEquals(problemsAfterImport(4), typeMismatch)
  }
}
