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

  test("goes to the declaration of the symbol under the cursor") {
    val locations = server.getTextDocumentService.definition(new DefinitionParams(document(main), new Position(6, 50)))
      .get(1, TimeUnit.MINUTES).getLeft.asScala.toSeq
    assertEquals(locations.map(l => (l.getUri, l.getRange.getStart)), Seq((greeting.toUri.toString, new Position(3, 6))))
  }
}
