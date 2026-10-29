package scalapsihost

import ch.epfl.scala.bsp4j.*
import com.google.gson.{Gson, JsonElement, JsonObject}
import com.intellij.lang.annotation.HighlightSeverity
import org.eclipse.lsp4j.jsonrpc.Launcher

import java.net.{ConnectException, SocketException, StandardProtocolFamily, URI, UnixDomainSocketAddress}
import java.nio.channels.SocketChannel
import java.nio.file.{Files, NoSuchFileException, Path}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/**
 * What compiling a file of a module needs, as the build describes it: the module's own compiler (its jars), the
 * compile classpath including the class output of the modules it depends on, the module's class output (its other
 * files' classes) and its scalac options.
 */
final case class CompileSpec(
  scalaVersion: String,
  compilerJars: Seq[Path],
  classpath: Seq[Path],
  classDirectory: Path,
  scalacOptions: Seq[String],
)

/**
 * A Scala build target of an imported build: the module to add, its source files (Scala and Java), and how to
 * compile its files.
 */
final case class ImportedModule(spec: ModuleSpec, sources: Seq[Path], compile: CompileSpec)

/** A 0-based line and column, as build servers (and LSP) report positions. */
final case class LineColumn(line: Int, column: Int)

/** A problem the build's compiler reported for a source file. */
final case class CompilerDiagnostic(severity: HighlightSeverity, start: LineColumn, end: LineColumn, message: String)

/**
 * @param succeeded   the build server's compile status was OK (no errors)
 * @param diagnostics the compiler's problems by source file, as last published for each file and build target
 */
final case class CompileResult(succeeded: Boolean, diagnostics: Map[Path, Seq[CompilerDiagnostic]])

/**
 * An open connection to a build server, through the Build Server Protocol (as other editors, and IntelliJ's BSP
 * support, talk to builds): it imports the build's modules and compiles them.
 *
 * Only Scala targets (data kind `scala`) become modules: sbt's own build definition targets are left out. Classpath
 * entries that are directories (the class output of other targets) are dropped, since target dependencies are
 * modules' `dependsOn`.
 */
final class BuildConnection private[scalapsihost] (
  process: Process,
  server: BspImport.HostBuildServer,
  capabilities: BuildServerCapabilities,
  client: BspImport.DiagnosticsClient,
  timeoutMinutes: Long,
  sbtServer: Option[BspImport.OwnedSbtServer],
) extends AutoCloseable {
  import BspImport.gson

  // A local shutdown handshake needs seconds, not the minutes allowed for importing and compiling a build.
  private val ShutdownDeadlineSeconds = 2L
  private val closed = new AtomicBoolean()

  private def await[T](f: java.util.concurrent.CompletableFuture[T]): T = f.get(timeoutMinutes, TimeUnit.MINUTES)

  // The build's Scala targets, asked for once per build definition (again after a reload).
  @volatile private var targets: Option[Seq[BuildTarget]] = None

  private def scalaTargets: Seq[BuildTarget] = targets.getOrElse {
    val found = await(server.workspaceBuildTargets()).getTargets.asScala.toSeq
      .filter(_.getDataKind == BuildTargetDataKind.SCALA)
      .sortBy(_.getId.getUri)
    targets = Some(found)
    found
  }

  /** The Scala modules, each after the modules it depends on. */
  def modules(): Seq[ImportedModule] = {
    val ids = scalaTargets.map(_.getId).asJava
    val optionsById = await(server.buildTargetScalacOptions(new ScalacOptionsParams(ids))).getItems.asScala
      .map(item => item.getTarget -> item).toMap
    val sourcesById = await(server.buildTargetSources(new SourcesParams(ids))).getItems.asScala
      .map(item => item.getTarget -> item).toMap
    val nameById = scalaTargets.map(t => t.getId -> BspImport.moduleName(t)).toMap

    val modules = scalaTargets.map { target =>
      val scala = gson.fromJson(target.getData.asInstanceOf[JsonElement], classOf[ScalaBuildTarget])
      val options = optionsById.getOrElse(target.getId,
        throw new IllegalStateException(s"build server sent no scalac options for ${target.getId.getUri}"))
      val fullClasspath = options.getClasspath.asScala.toSeq.map(uri => Path.of(URI.create(uri)))
      val spec = ModuleSpec(
        name = nameById(target.getId),
        scalaVersion = scala.getScalaVersion,
        scalacOptions = options.getOptions.asScala.toSeq,
        classpath = fullClasspath.filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".jar")),
        // Dependencies on non-Scala targets have no module.
        dependsOn = target.getDependencies.asScala.toSeq.flatMap(nameById.get),
      )
      val compile = CompileSpec(
        scalaVersion = scala.getScalaVersion,
        compilerJars = scala.getJars.asScala.toSeq.map(uri => Path.of(URI.create(uri))),
        classpath = fullClasspath,
        classDirectory = Path.of(URI.create(options.getClassDirectory)),
        scalacOptions = options.getOptions.asScala.toSeq,
      )
      ImportedModule(spec, sourcesById.get(target.getId).toSeq.flatMap(BspImport.sourceFiles), compile)
    }
    BspImport.dependenciesFirst(modules)
  }

  /** Reloads the build's definition (after a change to it); the next [[modules]] describe the reloaded build. */
  def reload(): Unit = {
    await(server.workspaceReload())
    targets = None
  }

  /**
   * The source jars of the Scala modules' classpath jars, as the build server provides them (sbt downloads them on
   * this request): classpath jar → its source jar. A source jar is paired with the classpath jar of the same name
   * without `-sources`, next to it (as dependency caches lay them out); the others are left out. Nothing when the
   * server does not provide dependency sources.
   */
  def librarySources(classpath: Set[Path]): Map[Path, Path] =
    if (!Option(capabilities.getDependencySourcesProvider).exists(_.booleanValue)) Map.empty
    else {
      val result = await(server.buildTargetDependencySources(new DependencySourcesParams(scalaTargets.map(_.getId).asJava)))
      val sourceJars = result.getItems.asScala.toSeq.flatMap(_.getSources.asScala).map(uri => Path.of(URI.create(uri))).distinct
      sourceJars.flatMap { sources =>
        val name = sources.getFileName.toString
        Option.when(name.endsWith("-sources.jar"))(sources.resolveSibling(name.stripSuffix("-sources.jar") + ".jar"))
          .filter(classpath.contains).map(_ -> sources)
      }.toMap
    }

  /**
   * Compiles every Scala target (incrementally, as the build server does) and returns the compiler's problems by
   * file. The server publishes them during the compile request, so they have all arrived when it answers.
   */
  def compile(): CompileResult = {
    val succeeded =
      try await(server.buildTargetCompile(new CompileParams(scalaTargets.map(_.getId).asJava))).getStatusCode == StatusCode.OK
      catch {
        // sbt answers a compile that failed with an error response instead of an ERROR status code; its problems were
        // published before the answer, like those of any compile.
        case e: java.util.concurrent.ExecutionException if e.getCause.isInstanceOf[org.eclipse.lsp4j.jsonrpc.ResponseErrorException] => false
      }
    CompileResult(succeeded, client.snapshot())
  }

  override def close(): Unit = if (closed.compareAndSet(false, true)) {
    var descendants = Seq.empty[ProcessHandle]
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(ShutdownDeadlineSeconds)
    try {
      // Capture children before the handshake: they may be reparented when the launcher exits normally.
      descendants = Using.resource(process.descendants())(_.iterator().asScala.toVector)
      server.buildShutdown().get(ShutdownDeadlineSeconds, TimeUnit.SECONDS)
      server.onBuildExit()
      process.waitFor(math.max(0L, deadline - System.nanoTime()), TimeUnit.NANOSECONDS)
    } catch {
      case e: InterruptedException =>
        HostLog.warn("interrupted while shutting down the build server", e)
        Thread.currentThread().interrupt()
      case NonFatal(e) => HostLog.warn("build server shutdown failed", e)
    } finally {
      BspImport.destroyProcess(process, descendants)
      sbtServer.foreach(_.stop())
    }
  }
}

object BspImport {
  private[scalapsihost] val gson = new Gson
  // The protocol version of the bsp4j on the classpath (the Scala plugin's).
  private val BspVersion = "2.1.0"

  /** The build server's interfaces the host calls. */
  trait HostBuildServer extends BuildServer with ScalaBuildServer

  private[scalapsihost] enum SbtServerState {
    case Missing
    case Inactive(socket: Path)
    case Live(socket: Path)
    case Unknown(reason: String)
  }

  /** A leftover socket is normal after sbt exits; a nonblocking connection checks liveness without delaying exit. */
  private[scalapsihost] def sbtServerState(projectDir: Path): SbtServerState = {
    import SbtServerState.*
    val active = projectDir.resolve("project/target/active.json")
    try {
      val json = gson.fromJson(Files.readString(active), classOf[JsonObject])
      val uri = Option(json).flatMap(j => Option(j.get("uri")))
        .filter(j => j.isJsonPrimitive && j.getAsJsonPrimitive.isString)
        .map(j => URI.create(j.getAsString))
        .getOrElse(throw new IllegalArgumentException(s"no server URI in $active"))
      if (uri.getScheme != "local" || uri.getAuthority != null || Option(uri.getPath).forall(!_.startsWith("/")))
        Unknown(s"cannot check server URI $uri")
      else {
        val socket = Path.of(uri.getPath)
        try {
          Using.resource(SocketChannel.open(StandardProtocolFamily.UNIX)) { channel =>
            channel.configureBlocking(false)
            if (channel.connect(UnixDomainSocketAddress.of(socket))) Live(socket)
            else Unknown(s"connection to $socket is pending")
          }
        } catch {
          case _: ConnectException | _: NoSuchFileException => Inactive(socket)
          // The JDK also reports a missing Unix socket as SocketException; other socket errors remain uncertain.
          case _: SocketException if Files.notExists(socket) => Inactive(socket)
        }
      }
    } catch {
      case _: NoSuchFileException => Missing
      case NonFatal(e) =>
        HostLog.warn(s"could not check sbt server in $active", e)
        Unknown(e.toString)
    }
  }

  /** The shutdown client can start a server itself, so it is only safe while our server is still live. */
  private[scalapsihost] final class OwnedSbtServer(projectDir: Path, argv: Seq[String], log: Path) {
    // With the two-second BSP handshake this leaves half a second of the LSP's five seconds for local cleanup.
    private val ServerShutdownDeadlineMillis = 2500L
    private val stopped = new AtomicBoolean()

    def stop(): Unit = if (stopped.compareAndSet(false, true)) {
      val deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(ServerShutdownDeadlineMillis)
      // An interrupted BSP handshake must still allow this bounded cleanup to reach the daemon.
      val interrupted = Thread.interrupted()
      try sbtServerState(projectDir) match {
        case SbtServerState.Live(_) =>
          HostLog.info(s"stopping sbt server started by this host in $projectDir")
          try {
            val shutdown = new ProcessBuilder((argv.dropRight(1) ++ Seq("--client", "shutdown")).asJava)
              .directory(projectDir.toFile)
              .redirectErrorStream(true)
              .redirectOutput(ProcessBuilder.Redirect.appendTo(log.toFile))
              .start()
            try {
              shutdown.getOutputStream.close()
              if (!shutdown.waitFor(math.max(0L, deadline - System.nanoTime()), TimeUnit.NANOSECONDS))
                HostLog.warn(s"sbt server shutdown exceeded $ServerShutdownDeadlineMillis ms (output: $log)")
              else if (shutdown.exitValue() != 0)
                HostLog.warn(s"sbt server shutdown exited with ${shutdown.exitValue()} (output: $log)")
            } finally destroyProcess(shutdown)
          } catch {
            case e: InterruptedException =>
              HostLog.warn("interrupted while stopping the owned sbt server", e)
              Thread.currentThread().interrupt()
            case NonFatal(e) => HostLog.warn(s"could not stop the owned sbt server (output: $log)", e)
          }
        case state @ SbtServerState.Unknown(_) => HostLog.warn(s"cannot safely stop the owned sbt server in $projectDir: $state")
        case state => HostLog.info(s"skipping owned sbt server shutdown in $projectDir: $state")
      } finally if (interrupted) Thread.currentThread().interrupt()
    }
  }

  /**
   * Starts the build server named by a connection file in `<project>/.bsp` and initializes it.
   *
   * @param connection the connection file's name (e.g. `sbt`) when `.bsp` has several; with one it may be omitted
   */
  def connect(projectDir: Path, connection: Option[String] = None, timeoutMinutes: Long = 10): BuildConnection = {
    val details = connectionDetails(projectDir, connection)
    val log = Files.createTempFile("scala-psi-host-bsp", ".log")
    val argv = details.getArgv.asScala.toSeq
    val sbtServer = if (details.getName == "sbt" && argv.lastOption.contains("-bsp")) {
      val state = sbtServerState(projectDir)
      val owns = state match {
        case SbtServerState.Missing | SbtServerState.Inactive(_) => true
        case _ => false
      }
      HostLog.info(s"sbt server before connecting in $projectDir: $state; host owns server: $owns")
      Option.when(owns)(new OwnedSbtServer(projectDir, argv, log))
    } else None
    HostLog.info(s"starting build server ${details.getName} ${details.getVersion}: ${details.getArgv.asScala.mkString(" ")} (its output: $log)")
    val process = new ProcessBuilder(details.getArgv)
      .directory(projectDir.toFile)
      .redirectError(ProcessBuilder.Redirect.to(log.toFile))
      .start()
    // SIGTERM can arrive during initialization, before a ProjectSession owns this process.
    val shutdownHook = new Thread(() => destroyProcess(process), s"scala-psi-host-bsp-${process.pid()}")
    try {
      Runtime.getRuntime.addShutdownHook(shutdownHook)
      process.onExit().thenRun(() => {
        try Runtime.getRuntime.removeShutdownHook(shutdownHook)
        catch { case e: IllegalStateException => HostLog.debug(s"build server exited during JVM shutdown: $e") }
      })
      val client = new DiagnosticsClient
      val launcher = new Launcher.Builder[HostBuildServer]()
        .setRemoteInterface(classOf[HostBuildServer])
        .setInput(process.getInputStream)
        .setOutput(process.getOutputStream)
        .setLocalService(client)
        .create()
      launcher.startListening()
      val server = launcher.getRemoteProxy
      val initialized = server.buildInitialize(new InitializeBuildParams(
        "scala-psi-host", "0.1", BspVersion, projectDir.toUri.toString,
        new BuildClientCapabilities(java.util.List.of("scala")))).get(timeoutMinutes, TimeUnit.MINUTES)
      server.onBuildInitialized()
      // A build server that was already running (sbt keeps its server between clients) may hold an older build
      // definition: reload it, as IDEs do on import.
      server.workspaceReload().get(timeoutMinutes, TimeUnit.MINUTES)
      new BuildConnection(process, server, initialized.getCapabilities, client, timeoutMinutes, sbtServer)
    } catch {
      case e: Throwable =>
        destroyProcess(process)
        sbtServer.foreach(_.stop())
        throw e
    }
  }

  /** Kills children before their parent so a BSP launcher cannot leave an orphaned build process behind. */
  private[scalapsihost] def destroyProcess(process: Process, descendants: Seq[ProcessHandle] = Seq.empty): Unit = {
    try {
      val current = Using.resource(process.descendants())(_.iterator().asScala.toVector)
      for (child <- (descendants ++ current).distinct.reverse) {
        try { if (child.isAlive) child.destroyForcibly() }
        catch { case NonFatal(e) => HostLog.warn(s"could not kill build server descendant ${child.pid()}", e) }
      }
    } catch { case NonFatal(e) => HostLog.warn(s"could not enumerate build server descendants of ${process.pid()}", e) }
    finally {
      try { if (process.isAlive) process.destroyForcibly() }
      catch { case NonFatal(e) => HostLog.warn(s"could not kill build server ${process.pid()}", e) }
    }
  }

  /** Imports the build's Scala modules, each after the modules it depends on, and disconnects. */
  def importBuild(projectDir: Path, connection: Option[String] = None, timeoutMinutes: Long = 10): Seq[ImportedModule] =
    Using.resource(connect(projectDir, connection, timeoutMinutes))(_.modules())

  /** Each module after the modules it depends on, so they can be added to the host in order. */
  private[scalapsihost] def dependenciesFirst(modules: Seq[ImportedModule]): Seq[ImportedModule] = {
    val ordered = scala.collection.mutable.LinkedHashMap.empty[String, ImportedModule]
    var waiting = modules
    while (waiting.nonEmpty) {
      val (ready, blocked) = waiting.partition(_.spec.dependsOn.forall(ordered.contains))
      if (ready.isEmpty) throw new IllegalStateException(s"cyclic module dependencies among ${blocked.map(_.spec.name).mkString(", ")}")
      ready.foreach(m => ordered(m.spec.name) = m)
      waiting = blocked
    }
    ordered.values.toSeq
  }

  private def connectionDetails(projectDir: Path, connection: Option[String]): BspConnectionDetails = {
    val bspDir = projectDir.resolve(".bsp")
    val files =
      if (!Files.isDirectory(bspDir)) Seq.empty
      else Using.resource(Files.list(bspDir))(_.iterator().asScala.filter(_.toString.endsWith(".json")).toSeq.sortBy(_.toString))
    val chosen = connection match {
      case Some(name) => files.find(_.getFileName.toString == s"$name.json")
        .getOrElse(throw new IllegalArgumentException(s"no BSP connection file $name.json in $bspDir"))
      case None => files match {
        case Seq(only) => only
        case Seq() => throw new IllegalArgumentException(s"no BSP connection file in $bspDir (for sbt: run `sbt bspConfig`)")
        case several => throw new IllegalArgumentException(
          s"several BSP connection files in $bspDir, choose one: ${several.map(_.getFileName).mkString(", ")}")
      }
    }
    gson.fromJson(Files.readString(chosen), classOf[BspConnectionDetails])
  }

  // Display names are what builds show users (sbt: `core`, `core-test`); the URI is unique when there is none.
  private[scalapsihost] def moduleName(target: BuildTarget): String = Option(target.getDisplayName).getOrElse(target.getId.getUri)

  private def isSource(path: Path) = path.toString.endsWith(".scala") || path.toString.endsWith(".java")

  private[scalapsihost] def sourceFiles(item: SourcesItem): Seq[Path] =
    item.getSources.asScala.toSeq.flatMap { source =>
      val path = Path.of(URI.create(source.getUri))
      if (!Files.exists(path)) Seq.empty
      else if (Files.isDirectory(path))
        Using.resource(Files.walk(path))(_.iterator().asScala.filter(isSource).toVector.sorted)
      else if (isSource(path)) Seq(path)
      else Seq.empty
    }

  /**
   * Keeps the compiler problems the server publishes, per file and build target: a notification with `reset`
   * replaces that pair's problems, one without adds to them (BSP `build/publishDiagnostics`). Logs and progress are
   * not used.
   */
  private[scalapsihost] final class DiagnosticsClient extends BuildClient {
    private val published = new ConcurrentHashMap[(Path, BuildTargetIdentifier), Vector[CompilerDiagnostic]]()

    def snapshot(): Map[Path, Seq[CompilerDiagnostic]] =
      published.asScala.toSeq.groupMap(_._1._1)(_._2).view.mapValues(_.flatten).filter(_._2.nonEmpty).toMap

    override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
      val key = (Path.of(URI.create(params.getTextDocument.getUri)), params.getBuildTarget)
      val problems = params.getDiagnostics.asScala.toVector.map { d =>
        val range = d.getRange
        CompilerDiagnostic(
          severity(d.getSeverity),
          LineColumn(range.getStart.getLine, range.getStart.getCharacter),
          LineColumn(range.getEnd.getLine, range.getEnd.getCharacter),
          d.getMessage)
      }
      if (params.getReset) published.put(key, problems)
      else published.merge(key, problems, _ ++ _)
    }

    private def severity(s: DiagnosticSeverity): HighlightSeverity = s match {
      case DiagnosticSeverity.ERROR => HighlightSeverity.ERROR
      case DiagnosticSeverity.WARNING => HighlightSeverity.WARNING
      // Absent severity: LSP lets the client decide; the IDE treats compiler output without one as information.
      case DiagnosticSeverity.INFORMATION | DiagnosticSeverity.HINT | null => HighlightSeverity.INFORMATION
    }

    override def onBuildShowMessage(params: ShowMessageParams): Unit = ()
    override def onBuildLogMessage(params: LogMessageParams): Unit = ()
    override def onBuildTaskStart(params: TaskStartParams): Unit = ()
    override def onBuildTaskProgress(params: TaskProgressParams): Unit = ()
    override def onBuildTaskFinish(params: TaskFinishParams): Unit = ()
    override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ()

    // sbt's own notifications, outside BSP: received so that the JSON-RPC layer does not report them as unsupported.
    @org.eclipse.lsp4j.jsonrpc.services.JsonNotification("sbt/exec")
    def onSbtExec(params: AnyRef): Unit = ()
    @org.eclipse.lsp4j.jsonrpc.services.JsonNotification("shutdown")
    def onSbtShutdown(params: AnyRef): Unit = ()
  }
}
