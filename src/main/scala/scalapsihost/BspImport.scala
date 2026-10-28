package scalapsihost

import ch.epfl.scala.bsp4j.*
import com.google.gson.{Gson, JsonElement}
import com.intellij.lang.annotation.HighlightSeverity
import org.eclipse.lsp4j.jsonrpc.Launcher

import java.net.URI
import java.nio.file.{Files, Path}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** A Scala build target of an imported build: the module to add, and its source files (Scala and Java). */
final case class ImportedModule(spec: ModuleSpec, sources: Seq[Path])

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
  client: BspImport.DiagnosticsClient,
  timeoutMinutes: Long,
) extends AutoCloseable {
  import BspImport.gson

  private def await[T](f: java.util.concurrent.CompletableFuture[T]): T = f.get(timeoutMinutes, TimeUnit.MINUTES)

  private lazy val scalaTargets: Seq[BuildTarget] =
    await(server.workspaceBuildTargets()).getTargets.asScala.toSeq
      .filter(_.getDataKind == BuildTargetDataKind.SCALA)
      .sortBy(_.getId.getUri)

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
      val classpath = options.getClasspath.asScala.toSeq.map(uri => Path.of(URI.create(uri)))
        .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".jar"))
      val spec = ModuleSpec(
        name = nameById(target.getId),
        scalaVersion = scala.getScalaVersion,
        scalacOptions = options.getOptions.asScala.toSeq,
        classpath = classpath,
        // Dependencies on non-Scala targets have no module.
        dependsOn = target.getDependencies.asScala.toSeq.flatMap(nameById.get),
      )
      ImportedModule(spec, sourcesById.get(target.getId).toSeq.flatMap(BspImport.sourceFiles))
    }
    BspImport.dependenciesFirst(modules)
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

  override def close(): Unit =
    try {
      await(server.buildShutdown())
      server.onBuildExit()
    } finally {
      if (!process.waitFor(30, TimeUnit.SECONDS)) process.destroyForcibly()
    }
}

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
    val process = new ProcessBuilder(details.getArgv)
      .directory(projectDir.toFile)
      .redirectError(ProcessBuilder.Redirect.to(log.toFile))
      .start()
    val client = new DiagnosticsClient
    val launcher = new Launcher.Builder[HostBuildServer]()
      .setRemoteInterface(classOf[HostBuildServer])
      .setInput(process.getInputStream)
      .setOutput(process.getOutputStream)
      .setLocalService(client)
      .create()
    launcher.startListening()
    val server = launcher.getRemoteProxy
    server.buildInitialize(new InitializeBuildParams(
      "scala-psi-host", "0.1", BspVersion, projectDir.toUri.toString,
      new BuildClientCapabilities(java.util.List.of("scala")))).get(timeoutMinutes, TimeUnit.MINUTES)
    server.onBuildInitialized()
    new BuildConnection(process, server, client, timeoutMinutes)
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
  }
}
