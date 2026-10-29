package scalapsihost.lsp

import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.progress.ProcessCanceledException
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either as LspEither
import org.eclipse.lsp4j.services.*
import scalapsihost.{CallLog, HostLog, LineColumn, ScalaPsiHost}

import java.net.URI
import java.nio.file.Path
import java.util.concurrent.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * The language server: LSP over a [[ProjectSession]] of the workspace's build.
 *
 * Document notifications and requests run in order on one thread, so a request sees every change sent
 * before it; the project is opened there first, so `initialize` answers at once. Diagnostics are computed on
 * another thread, a short while after a document's last change: a change cancels a computation in progress (the
 * host's write lock cancels running queries), and a result is published only for the document's current version.
 *
 * What the server does is logged ([[HostLog]]) and sent to the client as log messages; opening and importing the
 * project are reported as progress, and its state (see [[HostProtocol]]) as it changes. Requests are timed: slow ones
 * are logged, and every one at debug level.
 *
 * @param exitProcess ends the server's process with the given code when the client exits or disappears (0 after a
 *                    `shutdown` request, 1 otherwise, as LSP defines); nothing for a server embedded in another process
 * @param logFile     where the log is written, for messages that point to it
 * @param startupLog  what was logged before the client connected (the host's startup), sent to it when it connects
 */
final class ScalaLanguageServer(
  host: ScalaPsiHost,
  exitProcess: Int => Unit = _ => (),
  logFile: Option[Path] = None,
  startupLog: Seq[HostLog.Entry] = Seq.empty,
) extends LspServices.Server {
  // Fast typing produces a change per keystroke; diagnostics wait for this pause before they are computed.
  private val DiagnosticsDelayMs = 200L
  // A request that takes longer is logged as slow: an editor waiting this long looks stuck.
  private val SlowRequestMs = 1000L
  // Allows a short BSP shutdown handshake, but never waits for a full build import when the client has gone.
  private val EndingDeadlineSeconds = 5L

  @volatile private var client: LanguageClient = scala.compiletime.uninitialized
  private var clientLog: Option[AutoCloseable] = None
  @volatile private var completionOptions = scalapsihost.Completion.Options()
  @volatile private var watchesFiles = false
  @volatile private var reportsProgress = false
  @volatile private var state = new HostProtocol.ServerState("starting", "waiting for the client")
  private val startedAt = System.nanoTime()
  private val progressTokens = new AtomicInteger()
  private val requests = new RequestStats
  @volatile private var shutdownRequested = false
  @volatile private var projectRequested = false
  private val ending = new AtomicBoolean()
  private val documents = Executors.newSingleThreadExecutor(daemon("scala-psi-host-documents"))
  private val diagnostics = Executors.newSingleThreadScheduledExecutor(daemon("scala-psi-host-diagnostics"))
  private val session = new CompletableFuture[ProjectSession]
  // Each document's latest version, as the client numbers them; diagnostics of older versions are dropped.
  private val versions = new ConcurrentHashMap[Path, Integer]()
  // Changes are observed at receipt as well as application, so a queued request cannot publish an old completion.
  private val completionVersions = new ConcurrentHashMap[String, AtomicInteger]()
  private val completionGate = new Object
  private var completing = false

  private def completionVersion(uri: String): AtomicInteger = completionVersions.computeIfAbsent(uri, _ => new AtomicInteger())

  private def daemon(name: String): ThreadFactory = runnable => {
    val thread = new Thread(runnable, name)
    thread.setDaemon(true)
    thread
  }

  override def connect(client: LanguageClient): Unit = {
    this.client = client
    startupLog.foreach(sendLog)
    clientLog = Some(HostLog.addSink(sendLog))
  }

  private def sendLog(entry: HostLog.Entry): Unit = {
    val kind = entry.level match {
      case HostLog.Level.Error => MessageType.Error
      case HostLog.Level.Warn => MessageType.Warning
      case HostLog.Level.Info => MessageType.Info
      case HostLog.Level.Debug => MessageType.Log
    }
    client.logMessage(new MessageParams(kind, entry.toString))
  }

  private def setState(name: String, message: String): Unit = {
    state = new HostProtocol.ServerState(name, message)
    client match {
      case c: HostProtocol.Client => c.state(state)
      case _ => ()
    }
  }

  /** Runs `body` as a work-done progress the client shows (when it can), with `body`'s reports as its messages. */
  private def withProgress[T](title: String)(body: (String => Unit) => T): T =
    if (!reportsProgress) body(_ => ())
    else {
      val token = LspEither.forLeft[String, Integer](s"scala-psi-host-${progressTokens.incrementAndGet()}")
      // The client must know the token before progress is reported on it.
      client.createProgress(new WorkDoneProgressCreateParams(token)).get(30, TimeUnit.SECONDS)
      def send(value: WorkDoneProgressNotification) =
        client.notifyProgress(new ProgressParams(token, LspEither.forLeft[WorkDoneProgressNotification, Object](value)))
      val begin = new WorkDoneProgressBegin
      begin.setTitle(title)
      send(begin)
      try body { message =>
        val report = new WorkDoneProgressReport
        report.setMessage(message)
        send(report)
      }
      finally send(new WorkDoneProgressEnd)
    }

  /** Imports the build again (see [[ProjectSession.importBuild]]), as progress and server state. */
  private def importAgain(s: ProjectSession, reload: Boolean): Unit = {
    setState("importing", "importing the build again")
    try withProgress("scala-psi-host: importing the build")(progress => s.importBuild(reload, progress))
    finally setState("ready", s"${s.moduleCount} modules, ${s.sourceFiles.size} source files")
  }

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val offered = Option(params.getCapabilities)
    watchesFiles = offered.flatMap(c => Option(c.getWorkspace)).flatMap(w => Option(w.getDidChangeWatchedFiles))
      .flatMap(w => Option(w.getDynamicRegistration)).exists(_.booleanValue)
    reportsProgress = offered.flatMap(c => Option(c.getWindow)).flatMap(w => Option(w.getWorkDoneProgress)).exists(_.booleanValue)
    val completion = offered.flatMap(c => Option(c.getTextDocument)).flatMap(c => Option(c.getCompletion))
      .flatMap(c => Option(c.getCompletionItem))
    completionOptions = scalapsihost.Completion.Options(
      snippets = completion.flatMap(c => Option(c.getSnippetSupport)).exists(_.booleanValue),
      resolve = completion.flatMap(c => Option(c.getResolveSupport)).flatMap(c => Option(c.getProperties))
        .exists(_.contains("additionalTextEdits")))
    val folders = Option(params.getWorkspaceFolders).map(_.asScala.toSeq).getOrElse(Seq.empty)
    val rootUri = folders.headOption.map(_.getUri).orElse(Option(params.getRootUri))
      .getOrElse(throw new IllegalArgumentException("the client sent no workspace folder or root URI"))
    val clientName = Option(params.getClientInfo).map(i => s"${i.getName} ${Option(i.getVersion).getOrElse("")}".trim).getOrElse("an unnamed client")
    HostLog.info(s"initialized by $clientName for $rootUri")
    projectRequested = true
    documents.execute { () =>
      val root = Path.of(URI.create(rootUri))
      setState("importing", s"opening $root")
      try {
        val opened = withProgress("scala-psi-host: opening the project")(progress => ProjectSession.open(host, root, progress = progress))
        val heap = Runtime.getRuntime
        HostLog.info(s"ready in ${(System.nanoTime() - startedAt) / 1_000_000} ms since the server started: " +
          s"${opened.moduleCount} modules, ${opened.sourceFiles.size} source files, " +
          s"heap ${(heap.totalMemory - heap.freeMemory) / (1024 * 1024)} of ${heap.maxMemory / (1024 * 1024)} MB")
        setState("ready", s"${opened.moduleCount} modules, ${opened.sourceFiles.size} source files")
        session.complete(opened)
      } catch {
        case e: Throwable =>
          HostLog.error(s"opening the project at $root failed", e)
          setState("failed", String.valueOf(e.getMessage))
          val seeLog = logFile.fold("")(f => s" (log: $f)")
          client.showMessage(new MessageParams(MessageType.Error, s"scala-psi-host could not open the project: ${e.getMessage}$seeLog"))
          session.completeExceptionally(e)
      }
    }
    val capabilities = new ServerCapabilities
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
    capabilities.setHoverProvider(true)
    capabilities.setCompletionProvider(new CompletionOptions(true, java.util.List.of(".")))
    capabilities.setDocumentFormattingProvider(true)
    capabilities.setDocumentRangeFormattingProvider(true)
    capabilities.setDefinitionProvider(true)
    capabilities.setTypeDefinitionProvider(true)
    capabilities.setDocumentHighlightProvider(true)
    capabilities.setDocumentSymbolProvider(true)
    capabilities.setFoldingRangeProvider(true)
    capabilities.setSemanticTokensProvider(new SemanticTokensWithRegistrationOptions(SemanticColoring.legend, /* full = */ true))
    Option(params.getProcessId).foreach { pid =>
      try {
        val process = ProcessHandle.of(pid.longValue())
        if (process.isPresent) process.get().onExit().thenRun(() => exit())
        else exit()
      } catch { case NonFatal(e) => HostLog.warn(s"could not watch client process $pid", e) }
    }
    CompletableFuture.completedFuture(new InitializeResult(capabilities, new ServerInfo("scala-psi-host", "0.1")))
  }

  /**
   * Asks the client to report changes to the build's definition (sbt files, `project/`) and created or deleted source
   * files: either makes the server import the build again (see [[ProjectSession.importBuild]]).
   */
  override def initialized(params: InitializedParams): Unit =
    if (watchesFiles) {
      val changes = WatchKind.Create | WatchKind.Change | WatchKind.Delete
      val watchers = Seq(
        BuildDefinition.SbtFiles -> changes, BuildDefinition.ProjectFiles -> changes, BuildDefinition.BuildProperties -> changes,
        "**/*.{scala,java}" -> (WatchKind.Create | WatchKind.Delete),
      ).map((glob, kind) => new FileSystemWatcher(LspEither.forLeft[String, RelativePattern](glob), kind))
      client.registerCapability(new RegistrationParams(java.util.List.of(new Registration(
        "scala-psi-host-build-files", "workspace/didChangeWatchedFiles", new DidChangeWatchedFilesRegistrationOptions(watchers.asJava)))))
    }

  override def shutdown(): CompletableFuture[AnyRef] = {
    shutdownRequested = true
    closing
  }

  private lazy val closing: CompletableFuture[AnyRef] = {
    documents.shutdownNow()
    diagnostics.shutdownNow()
    val closed = if (!projectRequested) CompletableFuture.completedFuture[AnyRef](null)
    else session.handleAsync[AnyRef]((opened, error) => {
      if (error == null) opened.close()
      null
    }, runnable => daemon("scala-psi-host-ending").newThread(runnable).start())
    closed.orTimeout(EndingDeadlineSeconds, TimeUnit.SECONDS)
  }

  override def exit(): Unit = if (ending.compareAndSet(false, true)) {
    clientLog.foreach(_.close())
    HostLog.info(s"exiting (${if (shutdownRequested) "after a shutdown request" else "without a shutdown request"})")
    try closing.get(EndingDeadlineSeconds, TimeUnit.SECONDS)
    catch {
      case e: InterruptedException =>
        HostLog.warn("interrupted while closing the project", e)
        Thread.currentThread().interrupt()
      case NonFatal(e) => HostLog.warn("could not close the project before exiting", e)
    } finally exitProcess(if (shutdownRequested) 0 else 1)
  }

  /**
   * The server's status: its state, the project, memory, request latencies (since it started), the problems it logged
   * and the adapter calls outside the host's tables. Answered at once, also while the project opens.
   */
  override def status(): CompletableFuture[java.util.Map[String, Object]] = {
    def map(entries: (String, Any)*): java.util.Map[String, Object] = {
      val m = new java.util.LinkedHashMap[String, Object]()
      entries.foreach((k, v) => m.put(k, v.asInstanceOf[Object]))
      m
    }
    val heap = Runtime.getRuntime
    val opened = if (session.isDone && !session.isCompletedExceptionally) Some(session.join()) else None
    val project = opened.map(s =>
      map("modules" -> Integer.valueOf(s.moduleCount), "sourceFiles" -> Integer.valueOf(s.sourceFiles.size)))
    CompletableFuture.completedFuture(map(
      "state" -> state.state,
      "message" -> state.message,
      "uptimeSeconds" -> java.lang.Long.valueOf((System.nanoTime() - startedAt) / 1_000_000_000),
      "logFile" -> logFile.map(_.toString).orNull,
      "project" -> project.orNull,
      "heapMb" -> map("used" -> java.lang.Long.valueOf((heap.totalMemory - heap.freeMemory) / (1024 * 1024)),
        "max" -> java.lang.Long.valueOf(heap.maxMemory / (1024 * 1024))),
      "requests" -> map(requests.summary.map((kind, s) => kind -> map(
        "count" -> java.lang.Long.valueOf(s.count), "p50Ms" -> java.lang.Long.valueOf(s.p50),
        "p95Ms" -> java.lang.Long.valueOf(s.p95), "maxMs" -> java.lang.Long.valueOf(s.max)))*),
      "logged" -> map("errors" -> java.lang.Long.valueOf(HostLog.count(HostLog.Level.Error)),
        "warnings" -> java.lang.Long.valueOf(HostLog.count(HostLog.Level.Warn))),
      "unsupportedAdapterCalls" -> CallLog.snapshot.map(_._1).filter(_.startsWith("UNSUPPORTED ")).map(_.stripPrefix("UNSUPPORTED ")).asJava,
      "recentProblems" -> HostLog.recentProblems.takeRight(20).map(_.toString.linesIterator.next()).asJava,
    ))
  }

  override def getTextDocumentService: TextDocumentService = TextDocuments
  override def getWorkspaceService: WorkspaceService = Workspace

  // ---- Documents -------------------------------------------------------------------------------------------------

  private def pathOf(uri: String): Path = Path.of(URI.create(uri))

  /** A notification: handled in order on the documents thread; a failure is logged (a notification has no answer). */
  private def onDocuments(kind: String)(action: ProjectSession => Unit): Unit =
    documents.execute { () =>
      try timed(kind, System.nanoTime())(action(session.join()))
      catch { case NonFatal(e) => HostLog.error(s"$kind failed", e) }
    }

  /** A request: answered in order on the documents thread; a failure is logged and answered as an error. */
  private def onDocuments[T](kind: String)(request: ProjectSession => T): CompletableFuture[T] = {
    val submitted = System.nanoTime()
    CompletableFuture.supplyAsync(() =>
      try timed(kind, submitted)(request(session.join()))
      catch {
        case e: com.intellij.diff.comparison.DiffTooBigException =>
          HostLog.warn(s"$kind diff is too large", e)
          throw new org.eclipse.lsp4j.jsonrpc.ResponseErrorException(new org.eclipse.lsp4j.jsonrpc.messages.ResponseError(
            org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode.RequestFailed, "change diff is too large", null))
        case e: ProcessCanceledException => throw new org.eclipse.lsp4j.jsonrpc.ResponseErrorException(
          new org.eclipse.lsp4j.jsonrpc.messages.ResponseError(org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode.RequestCancelled,
            "request cancelled by a newer edit", null))
        case e: scalapsihost.ChangeEngine.UnsupportedFormatter =>
          client.showMessage(new MessageParams(MessageType.Error, e.getMessage))
          throw new org.eclipse.lsp4j.jsonrpc.ResponseErrorException(new org.eclipse.lsp4j.jsonrpc.messages.ResponseError(
            org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode.RequestFailed, e.getMessage, null))
        case NonFatal(e) =>
          HostLog.error(s"$kind failed", e)
          throw e
      }, documents)
  }

  // Times `body`, from `submitted` (waiting behind other work included, as the editor waits) and on its own.
  private def timed[T](kind: String, submitted: Long)(body: => T): T = {
    val started = System.nanoTime()
    try body
    finally {
      val (total, waited) = ((System.nanoTime() - submitted) / 1_000_000, (started - submitted) / 1_000_000)
      requests.record(kind, total)
      if (total >= SlowRequestMs) HostLog.warn(s"slow: $kind took $total ms ($waited ms waiting for other work)")
      else HostLog.debug(s"$kind: $total ms ($waited ms waiting)")
    }
  }

  // A project source file's path; none for other documents (a library file the editor opened, an unsaved buffer).
  private def sourcePath(s: ProjectSession, uri: String): Option[Path] =
    Option.when(uri.startsWith("file:"))(pathOf(uri)).filter(s.isSource)

  /** A request about a document: answered for project sources, `outside` for other documents. */
  private def onSource[T](kind: String, document: TextDocumentIdentifier, outside: => T)(request: (ProjectSession, Path) => T): CompletableFuture[T] =
    onDocuments(kind)(s => sourcePath(s, document.getUri).fold(outside)(request(s, _)))

  /** Receipt-time versions guard copy requests both before execution and before publishing their edits. */
  private def onCopyRequest[T](kind: String, document: TextDocumentIdentifier, outside: => T)
                              (request: (ProjectSession, Path) => T): CompletableFuture[T] = {
    val version = completionVersion(document.getUri)
    val submittedVersion = version.get()
    onSource(kind, document, outside) { (s, path) =>
      completionGate.synchronized { completing = true }
      try {
        if (version.get() != submittedVersion) throw new ProcessCanceledException
        val result = request(s, path)
        if (version.get() != submittedVersion) throw new ProcessCanceledException
        result
      } finally completionGate.synchronized { completing = false }
    }
  }

  private object TextDocuments extends LspServices.TextDocuments {
    // Documents that are not project sources are not tracked. A Scala or Java file that is not one yet may have just
    // been created (the editor may open it before reporting its creation): the build is imported again first.
    override def didOpen(params: DidOpenTextDocumentParams): Unit = onDocuments("didOpen") { s =>
      val uri = params.getTextDocument.getUri
      if (uri.startsWith("file:") && sourcePath(s, uri).isEmpty) {
        val path = pathOf(uri)
        val name = path.getFileName.toString
        if ((name.endsWith(".scala") || name.endsWith(".java")) && !path.startsWith(host.stateDir)) importAgain(s, reload = false)
      }
      sourcePath(s, uri).foreach { path =>
        versions.put(path, params.getTextDocument.getVersion)
        // The editor's buffer may differ from the file on disk (unsaved changes).
        if (s.text(path) != params.getTextDocument.getText) s.change(path, None, params.getTextDocument.getText)
        scheduleDiagnostics(path)
      }
    }

    override def didChange(params: DidChangeTextDocumentParams): Unit = {
      completionVersion(params.getTextDocument.getUri).incrementAndGet()
      // Cancel only during a copy request; offset reads used by another edit must finish.
      completionGate.synchronized { if (completing) host.cancelReads() }
      onDocuments("didChange") { s =>
        sourcePath(s, params.getTextDocument.getUri).foreach { path =>
          versions.put(path, params.getTextDocument.getVersion)
          for (change <- params.getContentChanges.asScala) {
            val range = Option(change.getRange).map(r => (lineColumn(r.getStart), lineColumn(r.getEnd)))
            s.change(path, range, change.getText)
          }
          scheduleDiagnostics(path)
        }
      }
    }

    override def didClose(params: DidCloseTextDocumentParams): Unit = ()

    // A save compiles the build: other files' compiler problems may change with it.
    override def didSave(params: DidSaveTextDocumentParams): Unit = onDocuments("compile on save") { s =>
      s.compileBuild().foreach(scheduleDiagnostics)
    }

    override def hover(params: HoverParams): CompletableFuture[Hover] = onSource("hover", params.getTextDocument, null) { (s, path) =>
      s.hover(path, lineColumn(params.getPosition)).map { signature =>
        new Hover(new MarkupContent(MarkupKind.MARKDOWN, s"```scala\n$signature\n```"))
      }.orNull
    }

    override def completion(params: CompletionParams)
        : CompletableFuture[LspEither[java.util.List[CompletionItem], CompletionList]] = {
      onCopyRequest("completion", params.getTextDocument,
        LspEither.forRight[java.util.List[CompletionItem], CompletionList](new CompletionList(false, java.util.List.of()))) { (s, path) =>
        LspEither.forRight(s.completion(path, lineColumn(params.getPosition),
          completionOptions.copy(uri = params.getTextDocument.getUri, version = Option(versions.get(path)).fold(0)(_.intValue))))
      }
    }

    override def resolveCompletionItem(item: CompletionItem): CompletableFuture[CompletionItem] =
      if (item.getData == null) CompletableFuture.completedFuture(item)
      else {
        val recipe = scalapsihost.Completion.recipe(item)
        onCopyRequest("completion resolve", new TextDocumentIdentifier(recipe.uri), throw new ProcessCanceledException) { (s, path) =>
          s.resolveCompletion(path, item, Option(versions.get(path)).fold(0)(_.intValue))
        }
      }

    override def formatting(params: DocumentFormattingParams): CompletableFuture[java.util.List[? <: TextEdit]] =
      onCopyRequest("formatting", params.getTextDocument, java.util.List.of[TextEdit]()) { (s, path) =>
        s.formatting(path, None, params.getOptions).asJava
      }

    override def rangeFormatting(params: DocumentRangeFormattingParams): CompletableFuture[java.util.List[? <: TextEdit]] =
      onCopyRequest("rangeFormatting", params.getTextDocument, java.util.List.of[TextEdit]()) { (s, path) =>
        val range = params.getRange
        s.formatting(path, Some(lineColumn(range.getStart) -> lineColumn(range.getEnd)), params.getOptions).asJava
      }

    override def definition(params: DefinitionParams)
        : CompletableFuture[LspEither[java.util.List[? <: Location], java.util.List[? <: LocationLink]]] =
      onSource("definition", params.getTextDocument, locations(Seq.empty)) { (s, path) =>
        locations(s.definition(path, lineColumn(params.getPosition)).toSeq)
      }

    override def typeDefinition(params: TypeDefinitionParams)
        : CompletableFuture[LspEither[java.util.List[? <: Location], java.util.List[? <: LocationLink]]] =
      onSource("typeDefinition", params.getTextDocument, locations(Seq.empty)) { (s, path) =>
        locations(s.typeDefinition(path, lineColumn(params.getPosition)))
      }

    private def locations(found: Seq[(Path, LineColumn)]): LspEither[java.util.List[? <: Location], java.util.List[? <: LocationLink]] =
      LspEither.forLeft(found.map { case (path, at) => new Location(path.toUri.toString, new Range(position(at), position(at))) }.asJava)

    override def documentHighlight(params: DocumentHighlightParams): CompletableFuture[java.util.List[? <: DocumentHighlight]] =
      onSource("documentHighlight", params.getTextDocument, java.util.List.of[DocumentHighlight]()) { (s, path) =>
        s.highlights(path, lineColumn(params.getPosition)).map { o =>
          new DocumentHighlight(range(o.range), if (o.write) DocumentHighlightKind.Write else DocumentHighlightKind.Read)
        }.asJava
      }

    override def documentSymbol(params: DocumentSymbolParams)
        : CompletableFuture[java.util.List[LspEither[SymbolInformation, DocumentSymbol]]] =
      onSource("documentSymbol", params.getTextDocument, java.util.List.of[LspEither[SymbolInformation, DocumentSymbol]]()) { (s, path) =>
        def symbol(o: OutlineSymbol): DocumentSymbol = {
          val d = new DocumentSymbol(o.name, o.kind, range(o.range), range(o.nameRange), o.detail)
          d.setChildren(o.children.map(symbol).asJava)
          d
        }
        s.outline(path).map(o => LspEither.forRight[SymbolInformation, DocumentSymbol](symbol(o))).asJava
      }

    override def semanticTokensFull(params: SemanticTokensParams): CompletableFuture[SemanticTokens] =
      onSource("semanticTokens", params.getTextDocument, new SemanticTokens(java.util.List.of[Integer]())) { (s, path) =>
        val data = SemanticColoring.encode(s.semanticTokens(path))
        new SemanticTokens(data.map(Integer.valueOf).asJava)
      }

    override def foldingRange(params: FoldingRangeRequestParams): CompletableFuture[java.util.List[FoldingRange]] =
      onSource("foldingRange", params.getTextDocument, java.util.List.of[FoldingRange]()) { (s, path) =>
        s.foldingRanges(path).map { fold =>
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
        // A file the build no longer has (deleted, or out of every module) has no problems left.
        val problems = timed("problems", System.nanoTime()) {
          if (session.join().isSource(path)) session.join().problems(path) else Seq.empty
        }
        if (isCurrent(path, version))
          client.publishDiagnostics(new PublishDiagnosticsParams(path.toUri.toString, problems.map(diagnostic).asJava, version))
      } catch {
        // A later change cancelled this computation; that change scheduled its own.
        case _: ProcessCanceledException => ()
        case NonFatal(e) => HostLog.error(s"problems of $path failed", e)
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

    // The build's definition changed (reload it), or source files were created or deleted: import the build again.
    // Every open document's problems may change with it.
    override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = onDocuments("build change") { s =>
      val paths = params.getChanges.asScala.toSeq.map(_.getUri).filter(_.startsWith("file:")).map(pathOf)
      val buildChanged = paths.exists(BuildDefinition.isPart)
      HostLog.info(s"${if (buildChanged) "build definition changed" else "source files created or deleted"}: ${paths.mkString(", ")}")
      importAgain(s, reload = buildChanged)
      versions.keySet.forEach(scheduleDiagnostics)
    }
  }
}

/** Which files define an sbt build: the client watches them, and a change to one reloads the build. */
private object BuildDefinition {
  val SbtFiles = "**/*.sbt"
  val ProjectFiles = "**/project/*.scala"
  val BuildProperties = "**/project/build.properties"

  def isPart(path: Path): Boolean = {
    val name = path.getFileName.toString
    val inProject = Option(path.getParent).exists(_.getFileName.toString == "project")
    name.endsWith(".sbt") || (inProject && (name.endsWith(".scala") || name == "build.properties"))
  }
}

/** Latencies by request kind: counts and maxima since the server started, percentiles over the latest requests. */
private final class RequestStats {
  // Enough recent requests of a kind for stable percentiles, few enough that memory stays flat.
  private val Window = 512

  final case class Summary(count: Long, p50: Long, p95: Long, max: Long)

  private final class Kind {
    var count = 0L
    var max = 0L
    val recent = new java.util.ArrayDeque[Long]()
  }

  private val kinds = new java.util.TreeMap[String, Kind]()

  def record(kind: String, ms: Long): Unit = kinds.synchronized {
    val k = kinds.computeIfAbsent(kind, _ => new Kind)
    k.count += 1
    k.max = math.max(k.max, ms)
    k.recent.addLast(ms)
    if (k.recent.size > Window) k.recent.removeFirst()
  }

  def summary: Seq[(String, Summary)] = kinds.synchronized {
    kinds.asScala.toSeq.map { (name, k) =>
      val sorted = k.recent.asScala.toVector.sorted
      def percentile(p: Double) = sorted(math.min(sorted.size - 1, (p * sorted.size).toInt))
      name -> Summary(k.count, percentile(0.5), percentile(0.95), k.max)
    }
  }
}
