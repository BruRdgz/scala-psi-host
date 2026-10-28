package scalapsihost

import ch.epfl.scala.bsp4j.*
import com.google.gson.{Gson, JsonElement}
import org.eclipse.lsp4j.jsonrpc.Launcher

import java.net.URI
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** A Scala build target of an imported build: the module to add, and its Scala source files. */
final case class ImportedModule(spec: ModuleSpec, sources: Seq[Path])

/**
 * Imports a build through the Build Server Protocol, as IDEs other than IntelliJ do (and IntelliJ's BSP import):
 * starts the build server named by a connection file in `<project>/.bsp`, asks it for the Scala build targets, their
 * scalac options, classpaths, dependencies and sources, and shuts it down.
 *
 * Only Scala targets (data kind `scala`) become modules: sbt's own build definition targets are left out. Classpath
 * entries that are directories (the class output of other targets) are dropped, since target dependencies are
 * modules' `dependsOn`. Java sources are not imported: the host has no Java source PSI.
 */
object BspImport {
  private val gson = new Gson
  // The protocol version of the bsp4j on the classpath (the Scala plugin's).
  private val BspVersion = "2.1.0"

  /** The build server's interfaces the host calls. */
  trait HostBuildServer extends BuildServer with ScalaBuildServer

  /**
   * @return the Scala modules, each after the modules it depends on
   * @param connection the connection file's name (e.g. `sbt`) when `.bsp` has several; with one it may be omitted
   */
  def importBuild(projectDir: Path, connection: Option[String] = None, timeoutMinutes: Long = 10): Seq[ImportedModule] = {
    val details = connectionDetails(projectDir, connection)
    val log = Files.createTempFile("scala-psi-host-bsp", ".log")
    val process = new ProcessBuilder(details.getArgv)
      .directory(projectDir.toFile)
      .redirectError(ProcessBuilder.Redirect.to(log.toFile))
      .start()
    try {
      val launcher = new Launcher.Builder[HostBuildServer]()
        .setRemoteInterface(classOf[HostBuildServer])
        .setInput(process.getInputStream)
        .setOutput(process.getOutputStream)
        .setLocalService(SilentClient)
        .create()
      launcher.startListening()
      val server = launcher.getRemoteProxy
      def await[T](f: java.util.concurrent.CompletableFuture[T]): T = f.get(timeoutMinutes, TimeUnit.MINUTES)

      await(server.buildInitialize(new InitializeBuildParams(
        "scala-psi-host", "0.1", BspVersion, projectDir.toUri.toString,
        new BuildClientCapabilities(java.util.List.of("scala")))))
      server.onBuildInitialized()

      val targets = await(server.workspaceBuildTargets()).getTargets.asScala.toSeq
        .filter(_.getDataKind == BuildTargetDataKind.SCALA)
        .sortBy(_.getId.getUri)
      val ids = targets.map(_.getId).asJava
      val optionsById = await(server.buildTargetScalacOptions(new ScalacOptionsParams(ids))).getItems.asScala
        .map(item => item.getTarget -> item).toMap
      val sourcesById = await(server.buildTargetSources(new SourcesParams(ids))).getItems.asScala
        .map(item => item.getTarget -> item).toMap
      val nameById = targets.map(t => t.getId -> moduleName(t)).toMap

      val modules = targets.map { target =>
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
        ImportedModule(spec, sourcesById.get(target.getId).toSeq.flatMap(scalaSources))
      }
      await(server.buildShutdown())
      server.onBuildExit()
      dependenciesFirst(modules)
    } finally {
      if (!process.waitFor(30, TimeUnit.SECONDS)) process.destroyForcibly()
    }
  }

  /** Each module after the modules it depends on, so they can be added to the host in order. */
  private def dependenciesFirst(modules: Seq[ImportedModule]): Seq[ImportedModule] = {
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
  private def moduleName(target: BuildTarget): String = Option(target.getDisplayName).getOrElse(target.getId.getUri)

  private def scalaSources(item: SourcesItem): Seq[Path] =
    item.getSources.asScala.toSeq.flatMap { source =>
      val path = Path.of(URI.create(source.getUri))
      if (!Files.exists(path)) Seq.empty
      else if (Files.isDirectory(path))
        Using.resource(Files.walk(path))(_.iterator().asScala.filter(_.toString.endsWith(".scala")).toVector.sorted)
      else if (path.toString.endsWith(".scala")) Seq(path)
      else Seq.empty
    }

  /** Server notifications (logs, progress, compile diagnostics) are not used by the import. */
  private object SilentClient extends BuildClient {
    override def onBuildShowMessage(params: ShowMessageParams): Unit = ()
    override def onBuildLogMessage(params: LogMessageParams): Unit = ()
    override def onBuildTaskStart(params: TaskStartParams): Unit = ()
    override def onBuildTaskProgress(params: TaskProgressParams): Unit = ()
    override def onBuildTaskFinish(params: TaskFinishParams): Unit = ()
    override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = ()
    override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ()
  }
}
