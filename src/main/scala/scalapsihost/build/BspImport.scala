package scalapsihost.build

import ch.epfl.scala.bsp4j.*
import com.google.gson.{Gson, JsonElement}
import com.intellij.lang.annotation.HighlightSeverity
import org.eclipse.lsp4j.jsonrpc.Launcher

import scalapsihost.{LineColumn, ModuleSpec}
import scalapsihost.platform.HostLog

import java.net.URI
import java.nio.file.{Files, Path}
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, ExecutionException, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

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
  log: Path,
  timeoutMinutes: Long,
) extends AutoCloseable {
  import BspImport.gson

  // A local shutdown handshake needs seconds, not the minutes allowed for importing and compiling a build.
  private val ShutdownDeadlineSeconds = 2L
  private val closed = new AtomicBoolean()

  /** Waits with the connection's import/compile deadline and reports early process exit with its log path. */
  private def await[T](f: CompletableFuture[T]): T = BspImport.await(process, log, f, timeoutMinutes)

  // The build's Scala targets, asked for once per build definition (again after a reload).
  @volatile private var targets: Option[Seq[BuildTarget]] = None

  /** Loads and caches Scala build targets until [[reload]] invalidates the build definition. */
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
    val identityById = scalaTargets.map(t => t.getId -> BspImport.targetIdentity(t)).toMap

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
        dependsOn = target.getDependencies.asScala.toSeq.flatMap(identityById.get),
        identity = Some(identityById(target.getId)),
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
  def compile(): scalapsihost.build.CompileResult = {
    val succeeded =
      try await(server.buildTargetCompile(new CompileParams(scalaTargets.map(_.getId).asJava))).getStatusCode == StatusCode.OK
      catch {
        // sbt answers a compile that failed with an error response instead of an ERROR status code; its problems were
        // published before the answer, like those of any compile.
        case e: java.util.concurrent.ExecutionException if e.getCause.isInstanceOf[org.eclipse.lsp4j.jsonrpc.ResponseErrorException] => false
      }
    scalapsihost.build.CompileResult(succeeded, client.snapshot())
  }

  /**
   * Ends the connection: the shutdown handshake, then the client process is killed if it overran it. The build's own
   * server (sbt keeps one per build directory, shared by its clients) is left running.
   */
  override def close(): Unit = if (closed.compareAndSet(false, true)) {
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(ShutdownDeadlineSeconds)
    try {
      server.buildShutdown().get(ShutdownDeadlineSeconds, TimeUnit.SECONDS)
      server.onBuildExit()
      process.waitFor(math.max(0L, deadline - System.nanoTime()), TimeUnit.NANOSECONDS)
    } catch {
      case e: InterruptedException =>
        HostLog.warn("interrupted while shutting down the build server", e)
        Thread.currentThread().interrupt()
      case NonFatal(e) => HostLog.warn("build server shutdown failed", e)
    } finally BspImport.destroyProcess(process)
  }
}

/** BSP connection setup and conversion of build-server results into host module and diagnostic models. */
object BspImport {
  private[scalapsihost] val gson = new Gson
  // The protocol version of the bsp4j on the classpath (the Scala plugin's).
  private val BspVersion = "2.1.0"

  /** The build server's interfaces the host calls. */
  trait HostBuildServer extends BuildServer with ScalaBuildServer

  /**
   * Starts the build server named by a connection file in `<project>/.bsp` and initializes it.
   *
   * @param connection the connection file's name (e.g. `sbt`) when `.bsp` has several; with one it may be omitted
   */
  def connect(projectDir: Path, connection: Option[String] = None, timeoutMinutes: Long = 10): BuildConnection = {
    val details = connectionDetails(projectDir, connection)
    val log = Files.createTempFile("scala-psi-host-bsp", ".log")
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
      val initialized = await(process, log, server.buildInitialize(new InitializeBuildParams(
        "scala-psi-host", "0.1", BspVersion, projectDir.toUri.toString,
        new BuildClientCapabilities(java.util.List.of("scala")))), timeoutMinutes)
      server.onBuildInitialized()
      // A build server that was already running (sbt keeps its server between clients) may hold an older build
      // definition: reload it, as IDEs do on import.
      await(process, log, server.workspaceReload(), timeoutMinutes)
      new BuildConnection(process, server, initialized.getCapabilities, client, log, timeoutMinutes)
    } catch {
      case e: Throwable =>
        destroyProcess(process)
        throw e
    }
  }

  /** The build server's process ended, so it will answer no request. */
  final class BuildServerExited(exitCode: Int, log: Path)
    extends RuntimeException(s"the build server exited (code $exitCode) without answering; its output: $log")

  /**
   * The build server's answer to a request. A server that exits never answers (sbt's exits when the build fails to
   * load), so its exit ends the wait rather than the timeout.
   */
  private[scalapsihost] def await[T](process: Process, log: Path, request: CompletableFuture[T], timeoutMinutes: Long): T = {
    val exited = process.onExit().thenApply[T](p => throw new BuildServerExited(p.exitValue(), log))
    try request.applyToEither(exited, answer => answer).get(timeoutMinutes, TimeUnit.MINUTES)
    catch { case e: ExecutionException if e.getCause.isInstanceOf[BuildServerExited] => throw e.getCause }
  }

  /** Forcefully ends the client process when initialization fails or its shutdown deadline expires. */
  private[scalapsihost] def destroyProcess(process: Process): Unit =
    try { if (process.isAlive) process.destroyForcibly() }
    catch { case NonFatal(e) => HostLog.warn(s"could not kill build server ${process.pid()}", e) }

  /** Imports the build's Scala modules, each after the modules it depends on, and disconnects. */
  def importBuild(projectDir: Path, connection: Option[String] = None, timeoutMinutes: Long = 10): Seq[ImportedModule] =
    Using.resource(connect(projectDir, connection, timeoutMinutes))(_.modules())

  /** Each module after the modules it depends on, so they can be added to the host in order. */
  private[scalapsihost] def dependenciesFirst(modules: Seq[ImportedModule]): Seq[ImportedModule] = {
    val byId = scala.collection.mutable.LinkedHashMap.empty[String, ImportedModule]
    modules.foreach { module =>
      val id = module.spec.stableId
      if (id == null || id.isBlank)
        throw new IllegalArgumentException(s"module ${module.spec.name} has an empty stable identity")
      if (byId.contains(id))
        throw new IllegalArgumentException(s"duplicate module identity $id (${module.spec.name})")
      byId(id) = module.copy(sources = module.sources.map(_.toAbsolutePath.normalize).distinct)
    }

    val sourceOwners = scala.collection.mutable.LinkedHashMap.empty[Path, ImportedModule]
    byId.values.foreach { module =>
      module.sources.map(_.toAbsolutePath.normalize).distinct.foreach { path =>
        sourceOwners.get(path) match {
          case Some(owner) if owner.spec.stableId != module.spec.stableId =>
            throw new IllegalArgumentException(
              s"source $path is assigned to multiple build targets: ${owner.spec.stableId} (${owner.spec.name}) and " +
                s"${module.spec.stableId} (${module.spec.name}); the host cannot choose one module for a shared source")
          case _ => sourceOwners(path) = module
        }
      }
    }

    val missing = byId.values.toSeq.flatMap { module =>
      module.spec.dependsOn.filterNot(byId.contains).map(id => module -> id)
    }
    if (missing.nonEmpty) {
      val details = missing.map { case (module, id) => s"${module.spec.stableId} (${module.spec.name}) -> $id" }
      throw new IllegalArgumentException(s"unknown module dependencies: ${details.mkString(", ")}")
    }

    val ordered = scala.collection.mutable.LinkedHashMap.empty[String, ImportedModule]
    var waiting = byId.values.toSeq
    while (waiting.nonEmpty) {
      val (ready, blocked) = waiting.partition(_.spec.dependsOn.forall(ordered.contains))
      if (ready.isEmpty) {
        val targets = blocked.map(m => s"${m.spec.stableId} (${m.spec.name})")
        throw new IllegalArgumentException(s"cyclic module dependencies among ${targets.mkString(", ")}")
      }
      ready.foreach(m => ordered(m.spec.stableId) = m)
      waiting = blocked
    }
    ordered.values.toSeq
  }

  /** Reads the selected `.bsp` connection file, requiring an explicit name when several files are present. */
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

  /** Uses the build's display name, falling back to its unique target URI when no name was supplied. */
  private[scalapsihost] def moduleName(target: BuildTarget): String =
    Option(target.getDisplayName).filter(_.nonEmpty).getOrElse(targetIdentity(target))

  /** The BSP target URI is stable across display-name collisions and build reloads. */
  private[scalapsihost] def targetIdentity(target: BuildTarget): String = {
    val uri = Option(target.getId).flatMap(id => Option(id.getUri)).filter(_.nonEmpty)
      .getOrElse(throw new IllegalArgumentException(s"build target has no identity URI: ${target.getDisplayName}"))
    uri
  }

  /** The only source extensions the host imports from BSP targets. */
  private def isSource(path: Path) = path.toString.endsWith(".scala") || path.toString.endsWith(".java")

  /** Resolves existing Scala and Java files from a BSP source item, expanding directory entries recursively. */
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

    /** Combines the latest diagnostics from all build targets by source path. */
    def snapshot(): Map[Path, Seq[CompilerDiagnostic]] =
      published.asScala.toSeq.groupMap(_._1._1)(_._2).view.mapValues(_.flatten).filter(_._2.nonEmpty).toMap

    /** Replaces or appends the diagnostics for one source and target, as indicated by BSP's reset flag. */
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

    /** Maps BSP diagnostic severities to IntelliJ highlighting levels. */
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

    /** Consumes sbt's out-of-BSP command notification; the host does not execute it. */
    @org.eclipse.lsp4j.jsonrpc.services.JsonNotification("sbt/exec")
    def onSbtExec(params: AnyRef): Unit = ()
    /** Consumes sbt's shutdown notification; connection shutdown remains owned by BSP. */
    @org.eclipse.lsp4j.jsonrpc.services.JsonNotification("shutdown")
    def onSbtShutdown(params: AnyRef): Unit = ()
  }
}
