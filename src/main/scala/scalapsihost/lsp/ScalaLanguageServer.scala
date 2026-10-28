package scalapsihost.lsp

import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.progress.ProcessCanceledException
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either as LspEither
import org.eclipse.lsp4j.services.*
import scalapsihost.{LineColumn, ScalaPsiHost}

import java.net.URI
import java.nio.file.Path
import java.util.concurrent.*
import scala.jdk.CollectionConverters.*

/**
 * The language server: LSP over a [[ProjectSession]] of the workspace's build.
 *
 * Document notifications and requests run in order on one thread, so a request sees every change sent
 * before it; the project is opened there first, so `initialize` answers at once. Diagnostics are computed on
 * another thread, a short while after a document's last change: a change cancels a computation in progress (the
 * host's write lock cancels running queries), and a result is published only for the document's current version.
 */
/**
 * @param exitProcess ends the server's process with the given code on the `exit` notification (0 after a `shutdown`
 *                    request, 1 otherwise, as LSP defines); nothing for a server embedded in another process
 */
final class ScalaLanguageServer(host: ScalaPsiHost, exitProcess: Int => Unit = _ => ()) extends LspServices.Server {
  // Fast typing produces a change per keystroke; diagnostics wait for this pause before they are computed.
  private val DiagnosticsDelayMs = 200L

  @volatile private var client: LanguageClient = scala.compiletime.uninitialized
  @volatile private var shutdownRequested = false
  private val documents = Executors.newSingleThreadExecutor(daemon("scala-psi-host-documents"))
  private val diagnostics = Executors.newSingleThreadScheduledExecutor(daemon("scala-psi-host-diagnostics"))
  private val session = new CompletableFuture[ProjectSession]
  // Each document's latest version, as the client numbers them; diagnostics of older versions are dropped.
  private val versions = new ConcurrentHashMap[Path, Integer]()

  private def daemon(name: String): ThreadFactory = runnable => {
    val thread = new Thread(runnable, name)
    thread.setDaemon(true)
    thread
  }

  override def connect(client: LanguageClient): Unit = this.client = client

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val folders = Option(params.getWorkspaceFolders).map(_.asScala.toSeq).getOrElse(Seq.empty)
    val rootUri = folders.headOption.map(_.getUri).orElse(Option(params.getRootUri))
      .getOrElse(throw new IllegalArgumentException("the client sent no workspace folder or root URI"))
    documents.execute(() =>
      try session.complete(ProjectSession.open(host, Path.of(URI.create(rootUri))))
      catch { case e: Throwable => session.completeExceptionally(e) })
    val capabilities = new ServerCapabilities
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
    capabilities.setHoverProvider(true)
    capabilities.setDefinitionProvider(true)
    capabilities.setTypeDefinitionProvider(true)
    capabilities.setDocumentHighlightProvider(true)
    capabilities.setDocumentSymbolProvider(true)
    capabilities.setFoldingRangeProvider(true)
    capabilities.setSemanticTokensProvider(new SemanticTokensWithRegistrationOptions(SemanticColoring.legend, /* full = */ true))
    CompletableFuture.completedFuture(new InitializeResult(capabilities, new ServerInfo("scala-psi-host", "0.1")))
  }

  override def initialized(params: InitializedParams): Unit = ()

  override def shutdown(): CompletableFuture[AnyRef] = {
    shutdownRequested = true
    CompletableFuture.supplyAsync(() => { if (session.isDone && !session.isCompletedExceptionally) session.join().close(); null }, documents)
  }

  override def exit(): Unit = {
    documents.shutdownNow()
    diagnostics.shutdownNow()
    exitProcess(if (shutdownRequested) 0 else 1)
  }

  override def getTextDocumentService: TextDocumentService = TextDocuments
  override def getWorkspaceService: WorkspaceService = Workspace

  // ---- Documents -------------------------------------------------------------------------------------------------

  private def pathOf(uri: String): Path = Path.of(URI.create(uri))

  private def onDocuments(action: ProjectSession => Unit): Unit =
    documents.execute(() => action(session.join()))

  private def onDocuments[T](request: ProjectSession => T): CompletableFuture[T] =
    CompletableFuture.supplyAsync(() => request(session.join()), documents)

  private object TextDocuments extends LspServices.TextDocuments {
    override def didOpen(params: DidOpenTextDocumentParams): Unit = onDocuments { s =>
      val path = pathOf(params.getTextDocument.getUri)
      versions.put(path, params.getTextDocument.getVersion)
      // The editor's buffer may differ from the file on disk (unsaved changes).
      if (s.text(path) != params.getTextDocument.getText) s.change(path, None, params.getTextDocument.getText)
      scheduleDiagnostics(path)
    }

    override def didChange(params: DidChangeTextDocumentParams): Unit = onDocuments { s =>
      val path = pathOf(params.getTextDocument.getUri)
      versions.put(path, params.getTextDocument.getVersion)
      for (change <- params.getContentChanges.asScala) {
        val range = Option(change.getRange).map(r => (lineColumn(r.getStart), lineColumn(r.getEnd)))
        s.change(path, range, change.getText)
      }
      scheduleDiagnostics(path)
    }

    override def didClose(params: DidCloseTextDocumentParams): Unit = ()

    // A save compiles the build: other files' compiler problems may change with it.
    override def didSave(params: DidSaveTextDocumentParams): Unit = onDocuments { s =>
      s.compileBuild().foreach(scheduleDiagnostics)
    }

    override def hover(params: HoverParams): CompletableFuture[Hover] = onDocuments { s =>
      s.hover(pathOf(params.getTextDocument.getUri), lineColumn(params.getPosition)).map { signature =>
        new Hover(new MarkupContent(MarkupKind.MARKDOWN, s"```scala\n$signature\n```"))
      }.orNull
    }

    override def definition(params: DefinitionParams)
        : CompletableFuture[LspEither[java.util.List[? <: Location], java.util.List[? <: LocationLink]]] = onDocuments { s =>
      val locations = s.definition(pathOf(params.getTextDocument.getUri), lineColumn(params.getPosition)).toList.map {
        case (path, at) => new Location(path.toUri.toString, new Range(position(at), position(at)))
      }
      LspEither.forLeft[java.util.List[? <: Location], java.util.List[? <: LocationLink]](locations.asJava)
    }

    override def typeDefinition(params: TypeDefinitionParams)
        : CompletableFuture[LspEither[java.util.List[? <: Location], java.util.List[? <: LocationLink]]] = onDocuments { s =>
      val locations = s.typeDefinition(pathOf(params.getTextDocument.getUri), lineColumn(params.getPosition)).toList.map {
        case (path, at) => new Location(path.toUri.toString, new Range(position(at), position(at)))
      }
      LspEither.forLeft[java.util.List[? <: Location], java.util.List[? <: LocationLink]](locations.asJava)
    }

    override def documentHighlight(params: DocumentHighlightParams): CompletableFuture[java.util.List[? <: DocumentHighlight]] =
      onDocuments { s =>
        s.highlights(pathOf(params.getTextDocument.getUri), lineColumn(params.getPosition)).map { o =>
          new DocumentHighlight(range(o.range), if (o.write) DocumentHighlightKind.Write else DocumentHighlightKind.Read)
        }.asJava
      }

    override def documentSymbol(params: DocumentSymbolParams)
        : CompletableFuture[java.util.List[LspEither[SymbolInformation, DocumentSymbol]]] = onDocuments { s =>
      def symbol(o: OutlineSymbol): DocumentSymbol = {
        val d = new DocumentSymbol(o.name, o.kind, range(o.range), range(o.nameRange), o.detail)
        d.setChildren(o.children.map(symbol).asJava)
        d
      }
      s.outline(pathOf(params.getTextDocument.getUri)).map(o => LspEither.forRight[SymbolInformation, DocumentSymbol](symbol(o))).asJava
    }

    override def semanticTokensFull(params: SemanticTokensParams): CompletableFuture[SemanticTokens] =
      onDocuments { s =>
        val data = SemanticColoring.encode(s.semanticTokens(pathOf(params.getTextDocument.getUri)))
        new SemanticTokens(data.map(Integer.valueOf).asJava)
      }

    override def foldingRange(params: FoldingRangeRequestParams): CompletableFuture[java.util.List[FoldingRange]] = onDocuments { s =>
      s.foldingRanges(pathOf(params.getTextDocument.getUri)).map { fold =>
        val r = new FoldingRange(fold.startLine, fold.endLine)
        fold.kind match {
          case Fold.Comment => r.setKind(FoldingRangeKind.Comment)
          case Fold.Imports => r.setKind(FoldingRangeKind.Imports)
          case Fold.Region => r.setKind(FoldingRangeKind.Region)
          case Fold.Code => ()
        }
        r
      }.asJava
    }
  }

  // ---- Diagnostics -----------------------------------------------------------------------------------------------

  private def scheduleDiagnostics(path: Path): Unit = {
    val version = versions.get(path)
    diagnostics.schedule((() => publishDiagnostics(path, version)): Runnable, DiagnosticsDelayMs, TimeUnit.MILLISECONDS)
  }

  private def isCurrent(path: Path, version: Integer): Boolean = versions.get(path) == version

  private def publishDiagnostics(path: Path, version: Integer): Unit =
    if (isCurrent(path, version)) {
      try {
        val problems = session.join().problems(path)
        if (isCurrent(path, version))
          client.publishDiagnostics(new PublishDiagnosticsParams(path.toUri.toString, problems.map(diagnostic).asJava, version))
      } catch {
        // A later change cancelled this computation; that change scheduled its own.
        case _: ProcessCanceledException => ()
      }
    }

  private def diagnostic(problem: FileProblem): Diagnostic = {
    val d = new Diagnostic(new Range(position(problem.start), position(problem.end)), problem.message)
    d.setSeverity(problem.severity match {
      case s if s.compareTo(HighlightSeverity.ERROR) >= 0 => DiagnosticSeverity.Error
      case s if s.compareTo(HighlightSeverity.WARNING) >= 0 => DiagnosticSeverity.Warning
      case _ => DiagnosticSeverity.Information
    })
    d.setSource(problem.source)
    d
  }

  private def lineColumn(p: Position): LineColumn = LineColumn(p.getLine, p.getCharacter)
  private def position(at: LineColumn): Position = new Position(at.line, at.column)
  private def range(span: (LineColumn, LineColumn)): Range = new Range(position(span._1), position(span._2))

  // ---- Workspace -------------------------------------------------------------------------------------------------

  private object Workspace extends LspServices.Workspace {
    override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = ()
    override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = ()
  }
}
