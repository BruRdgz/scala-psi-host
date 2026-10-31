package scalapsihost.workspace

import com.intellij.psi.PsiFile

import scalapsihost.{HostModule, LineColumn, ScalaPsiHost}
import scalapsihost.build.{BspImport, BuildConnection, CompileSpec, CompilerDiagnostic, DocumentCompiler, ImportedModule, ScalafmtFormatting}
import scalapsihost.language.{CodeActionResult, CompletionChoices, CompletionFollowUp, FileChanges, FileProblem, Fold, InlineHint, Occurrence, OutlineSymbol, ProjectLanguageOperations, SemanticToken, SignatureChoices, SourceSpan, TextChange, WorkspaceSymbolMatch}
import scalapsihost.platform.{HostLock, HostLog}

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import scala.util.Using

/**
 * An open project: its build (kept connected, to compile on save and to import again after a build change), its
 * modules in the host and every source file of them, loaded from disk and then kept in step with the editor's
 * changes. Positions are 0-based lines and columns in UTF-16 code units, as LSP counts them and as documents index
 * their text. Callers serialize session operations and imports; the LSP does this on its documents executor. The
 * diagnostics executor may call `isSource` and `problems` concurrently, and those methods share a guard with import
 * application so a diagnostic never reads the host while its module snapshot is only partly applied. Raw host PSI
 * queries still use the host's read/write lock.
 */
final class ProjectSession private (
  host: ScalaPsiHost,
  build: BuildConnection,
  root: Path,
) extends AutoCloseable {
  private val files = new ConcurrentHashMap[Path, PsiFile]()
  // Diagnostics run apart from the serialized documents executor; keep their source check/read outside a partial import.
  private val importSnapshotLock = new Object
  // The build's modules as last imported (changed only by `importBuild`, on the session's thread).
  @volatile private var imported: Seq[ImportedModule] = Seq.empty
  @volatile private var compileSpecs: Map[String, CompileSpec] = Map.empty
  // Classpath jars whose sources were asked for, attached or not.
  private var sourcesAskedFor = Set.empty[Path]
  private val documentCompiler = new DocumentCompiler
  private val scalafmt = new ScalafmtFormatting(host.stateDir.resolve("scalafmt"))
  // The build's compiler problems from its last compile, by file (for files the document compiler does not cover).
  private val buildProblems = new AtomicReference[Map[Path, Seq[CompilerDiagnostic]]](Map.empty)
  @volatile private var unavailable: Option[String] = None
  @volatile private var lastImportedSnapshot = ProjectSession.Snapshot(0, 0)

  private final case class ImportPlan(
    next: Seq[ImportedModule],
    before: Map[String, ImportedModule],
    after: Map[String, ImportedModule],
    replaced: Set[String],
    sourceText: Map[Path, String],
    newlyAskedForSources: Set[Path],
    librarySources: Map[Path, Path],
  )

  /** A snapshot of the source paths currently open in the session; their order is unspecified. */
  def sourceFiles: Seq[Path] = files.keySet.toArray(Array.empty[Path]).toSeq

  /** Whether `path` is a source file of the project (not, say, a library file the editor opened). */
  def isSource(path: Path): Boolean = importSnapshotLock.synchronized {
    ensureUsable()
    files.containsKey(path)
  }

  /** Whether a failed import changed host PSI far enough that this session can no longer answer source requests. */
  private[scalapsihost] def isUsable: Boolean = unavailable.isEmpty

  /** The reason source requests can no longer be served, if an import failed after host mutations began. */
  private[scalapsihost] def unavailableMessage: Option[String] = unavailable

  /** Counts from the last fully applied import, safe for the at-once status response during an update. */
  private[scalapsihost] def snapshot: ProjectSession.Snapshot = lastImportedSnapshot

  private def ensureUsable(): Unit = unavailable.foreach(message => throw new ProjectSession.Unavailable(message))

  private def markUnavailable(error: Throwable): Unit = {
    val detail = Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
    unavailable = Some(s"the project session is unusable after a partial build import ($detail); restart the language server to recover")
  }

  /** Resolves an open project source or reports that the path is not part of this session. */
  private def file(path: Path): PsiFile = {
    ensureUsable()
    Option(files.get(path)).getOrElse(throw new IllegalArgumentException(s"not a source file of the project: $path"))
  }

  private val language = new ProjectLanguageOperations(host, root, documentCompiler, scalafmt, file)

  /** The current in-memory document text, including changes not yet saved to disk. */
  def text(path: Path): String = language.text(path)

  /** The completion suggestions at `at`, best first. Only Scala files complete. */
  def completion(path: Path, at: LineColumn): CompletionChoices = language.completion(path, at)

  /** The plugin's inline hints within the requested range. Only Scala files have them. */
  def inlayHints(path: Path, start: LineColumn, end: LineColumn): Seq[InlineHint] = language.inlayHints(path, start, end)

  /** The signatures of the call around `at`. Only Scala files have them. */
  def signatures(path: Path, at: LineColumn): Option[SignatureChoices] = language.signatures(path, at)

  /** Each position's Extend Selection chain. An unexpandable caret still has its empty range, as LSP requires. */
  def selectionRanges(path: Path, positions: Seq[LineColumn]): Seq[Seq[(LineColumn, LineColumn)]] =
    language.selectionRanges(path, positions)

  /** Runs the insert handler of the chosen completion suggestion. */
  def insertCompletion(path: Path, start: LineColumn, prefixText: String, lookup: String, identity: String)
      : Option[CompletionFollowUp] = language.insertCompletion(path, start, prefixText, lookup, identity)

  /** The changes that format `range` of the file (all of it without one). Only Scala files are formatted. */
  def formatting(path: Path, range: Option[(LineColumn, LineColumn)], indentSize: Int, useTabs: Boolean): Seq[TextChange] =
    language.formatting(path, range, indentSize, useTabs)

  /** Applies an editor change to the current document. */
  def change(path: Path, range: Option[(LineColumn, LineColumn)], replacement: String): Unit =
    language.change(path, range, replacement)

  /** The file's plugin and compiler problems, from a consistent imported workspace snapshot. */
  def problems(path: Path): Seq[FileProblem] = importSnapshotLock.synchronized {
    val source = file(path)
    val spec = moduleSpec(source)
    language.problems(path, spec, buildProblems.get.getOrElse(path, Seq.empty))
  }

  /** The build compiler settings for the module that owns this source file. */
  private def moduleSpec(file: PsiFile): Option[CompileSpec] =
    host.modules.ofFile(file.getViewProvider.getVirtualFile).flatMap(m => compileSpecs.get(m.spec.stableId))

  /** Markdown signature and ScalaDoc at the requested position. */
  def hover(path: Path, at: LineColumn): Option[String] = language.hover(path, at)

  /** Where the symbol referenced at the requested position is declared in project sources. */
  def definition(path: Path, at: LineColumn): Option[(Path, LineColumn)] = language.definition(path, at)

  /** Where the type of the symbol at the requested position is declared. */
  def typeDefinition(path: Path, at: LineColumn): Seq[(Path, LineColumn)] = language.typeDefinition(path, at)

  /** Where the symbol is used in project sources. */
  def references(path: Path, at: LineColumn, includeDeclaration: Boolean): Seq[SourceSpan] =
    language.references(path, at, includeDeclaration)

  /** Applicable plugin actions, with edits against the current document. */
  def codeActions(path: Path, start: LineColumn, end: LineColumn, only: Seq[String] = Seq.empty)
      : Seq[(CodeActionResult, Seq[TextChange])] = language.codeActions(path, start, end, only)

  /** The range of the name at the requested position that a rename would change. */
  def prepareRename(path: Path, at: LineColumn): (LineColumn, LineColumn) = language.prepareRename(path, at)

  /** The source edits that rename the symbol at the requested position. */
  def rename(path: Path, at: LineColumn, newName: String): Seq[FileChanges] = language.rename(path, at, newName)

  /** The names of implementations and overriders of the symbol at the requested position. */
  def implementations(path: Path, at: LineColumn): Seq[SourceSpan] = language.implementations(path, at)

  /** The project's symbols whose names match `query`, best match first. */
  def workspaceSymbols(query: String): Seq[WorkspaceSymbolMatch] = {
    ensureUsable()
    language.workspaceSymbols(query)
  }

  /** The occurrences in this file of the symbol at the requested position. */
  def highlights(path: Path, at: LineColumn): Seq[Occurrence] = language.highlights(path, at)

  /** The file's outline. */
  def outline(path: Path): Seq[OutlineSymbol] = language.outline(path)

  /** The file's folding ranges. */
  def foldingRanges(path: Path): Seq[Fold] = language.foldingRanges(path)

  /** The file's semantic tokens. */
  def semanticTokens(path: Path): Seq[SemanticToken] = language.semanticTokens(path)

  /**
   * Imports the build's modules and applies them to the host: on opening, every module and its sources; after a
   * build change, the difference. Sources the build added are opened (from disk) and those it removed closed; a
   * module whose definition changed (Scala version, options, classpath, dependencies), and every module that
   * depends on it, is removed and added again, its files reopened with their current text (unsaved editor changes
   * are kept). `reload` first reloads the build's definition (after a change to it). Each step is logged, and
   * reported to `progress` as it starts.
   */
  def importBuild(reload: Boolean, progress: String => Unit = _ => ()): Unit = {
    ensureUsable()
    val start = System.nanoTime()
    if (reload) {
      progress("reloading the build definition")
      HostLog.timed("build definition reloaded")(build.reload())
    }
    progress("importing the build's modules")
    val modules = HostLog.timed("modules described by the build")(build.modules())
    val plan = planImport(modules, progress)
    importSnapshotLock.synchronized {
      var hostMutationStarted = false
      HostLock.write {
        try applyImport(plan, progress, () => hostMutationStarted = true)
        catch {
          case error: Throwable =>
            if (hostMutationStarted) markUnavailable(error)
            throw error
        }
      }
    }
    val readded = plan.replaced.intersect(plan.before.keySet).intersect(plan.after.keySet).size
    HostLog.info(s"build imported in ${(System.nanoTime() - start) / 1_000_000} ms: ${plan.next.size} modules " +
      s"(${plan.after.keySet.diff(plan.before.keySet).size} new, $readded added again, " +
      s"${plan.before.keySet.diff(plan.after.keySet).size} removed), " +
      s"${plan.next.map(_.sources.size).sum} imported sources, ${files.size} in all")
  }

  /** Validates the imported descriptors and gathers every source/build input before changing host PSI. */
  private def planImport(modules: Seq[ImportedModule], progress: String => Unit): ImportPlan = {
    val next = BspImport.dependenciesFirst(modules)
    val before = imported.map(m => m.spec.stableId -> m).toMap
    val after = next.map(m => m.spec.stableId -> m).toMap
    val changed = before.keySet.filter(id => !after.get(id).exists(_.spec == before(id).spec))

    /** Includes transitive dependents, which must be rebuilt because their scopes include changed modules. */
    def dependents(ids: Set[String]): Set[String] = {
      val more = ids ++ before.values.filter(_.spec.dependsOn.exists(ids)).map(_.spec.stableId)
      if (more == ids) ids else dependents(more)
    }

    val oldOwners = imported.flatMap(module => module.sources.map(_ -> module.spec.stableId)).toMap
    val newOwners = next.flatMap(module => module.sources.map(_ -> module.spec.stableId)).toMap
    val movedSources = oldOwners.keySet.intersect(newOwners.keySet).filter(path => oldOwners(path) != newOwners(path))
    val movedModules = movedSources.flatMap(path => Set(oldOwners(path), newOwners(path)))
    val replaced = dependents(changed ++ movedModules)

    val paths = next.flatMap(_.sources).distinct
    val openText = host.read {
      paths.flatMap(path => Option(files.get(path)).map(file => path -> file.getText))
    }.toMap
    val sourceText = paths.map(path => path -> openText.getOrElse(path, Files.readString(path))).toMap

    // Fetch source attachments before the PSI update, so a build-server failure cannot leave modules half replaced.
    val newlyAskedForSources = next.flatMap(_.spec.classpath).toSet -- sourcesAskedFor
    val librarySources = if (newlyAskedForSources.isEmpty) Map.empty
    else {
      progress(s"attaching library sources (${newlyAskedForSources.size} jars)")
      HostLog.timed(s"source jars asked for ${newlyAskedForSources.size} jars") {
        build.librarySources(newlyAskedForSources)
      }
    }
    ImportPlan(next, before, after, replaced, sourceText, newlyAskedForSources, librarySources)
  }

  /** Applies a validated plan. Any failure after the first host mutation leaves the session unavailable. */
  private def applyImport(plan: ImportPlan, progress: String => Unit, beforeMutation: () => Unit): Unit = {
    var closed = 0
    var opened = 0

    // Remove dependents first (reverse of the dependencies-first import order).
    for (module <- imported.reverse if plan.replaced(module.spec.stableId)) {
      for (source <- module.sources; file <- Option(files.get(source))) {
        beforeMutation()
        files.remove(source)
        host.closeSource(file)
        closed += 1
      }
      beforeMutation()
      host.removeModule(module.spec.stableId)
    }

    def openPlanned(source: Path, module: HostModule): Unit = {
      beforeMutation()
      files.put(source, host.openFile(source, plan.sourceText(source), module))
      opened += 1
    }

    for (moduleSpec <- plan.next) {
      val id = moduleSpec.spec.stableId
      if (!plan.before.contains(id) || plan.replaced(id)) {
        progress(s"module ${moduleSpec.spec.name} ($id): ${moduleSpec.spec.classpath.size} jars, ${moduleSpec.sources.size} sources")
        val module = HostLog.timed(
          s"module ${moduleSpec.spec.name} ($id, Scala ${moduleSpec.spec.scalaVersion}, ${moduleSpec.spec.classpath.size} jars) added") {
          beforeMutation()
          host.addModule(moduleSpec.spec)
        }
        moduleSpec.sources.foreach(source => openPlanned(source, module))
      } else {
        val module = host.modules.byId(id).getOrElse(
          throw new IllegalStateException(s"host module $id (${moduleSpec.spec.name}) disappeared during import"))
        val (kept, now) = (plan.before(id).sources.toSet, moduleSpec.sources.toSet)
        for (source <- kept -- now; file <- Option(files.get(source))) {
          beforeMutation()
          files.remove(source)
          host.closeSource(file)
          closed += 1
        }
        for (source <- now -- kept) openPlanned(source, module)
      }
    }

    for ((jar, sources) <- plan.librarySources) {
      beforeMutation()
      host.addLibrarySources(jar, sources)
    }
    if (plan.librarySources.nonEmpty)
      HostLog.info(s"library sources attached to ${plan.librarySources.size} of ${plan.newlyAskedForSources.size} jars")

    imported = plan.next
    compileSpecs = plan.next.map(module => module.spec.stableId -> module.compile).toMap
    sourcesAskedFor ++= plan.newlyAskedForSources
    lastImportedSnapshot = ProjectSession.Snapshot(imported.size, files.size)
    HostLog.info(s"${opened} sources opened, ${closed} closed, ${files.size} in all")
  }

  /** How many modules the build had at the last successful import. */
  def moduleCount: Int = imported.size

  /** Compiles the build, stores its latest compiler problems, and returns open files whose problems changed. */
  def compileBuild(): Set[Path] = {
    ensureUsable()
    val before = buildProblems.get
    val after = build.compile().diagnostics
    buildProblems.set(after)
    (before.keySet ++ after.keySet).filter(p => before.get(p) != after.get(p) && files.containsKey(p))
  }

  /** Closes the build connection, formatter resources and cached document-compiler loaders. */
  override def close(): Unit = Using.resources(build, scalafmt, documentCompiler)((_, _, _) => ())
}

object ProjectSession {
  private[scalapsihost] final case class Snapshot(moduleCount: Int, sourceFileCount: Int)

  /** A request cannot use a session whose host PSI was left partial by an import failure. */
  private[scalapsihost] final class Unavailable(message: String) extends IllegalStateException(message)

  /**
   * Imports the build at `root` over BSP, adds its modules to `host` and opens all their sources from disk. Steps
   * are logged and reported to `progress` as they start.
   */
  def open(host: ScalaPsiHost, root: Path, connection: Option[String] = None, progress: String => Unit = _ => ()): ProjectSession = {
    progress("connecting to the build server")
    val build = HostLog.timed(s"build server of $root connected")(BspImport.connect(root, connection))
    var openedSession = Option.empty[ProjectSession]
    try {
      val javaHome = Path.of(System.getProperty("java.home"))
      host.addJdk(javaHome)
      // The JDK's sources, as the IDE attaches them to the JDK.
      Some(javaHome.resolve("lib/src.zip")).filter(Files.isRegularFile(_)).foreach(host.addJdkSources)
      val session = new ProjectSession(host, build, root)
      openedSession = Some(session)
      session.importBuild(reload = false, progress)
      session
    } catch {
      case error: Throwable =>
        val resource: AutoCloseable = openedSession match {
          case Some(session) => session
          case None => build
        }
        try resource.close()
        catch { case closeError: Throwable => if (closeError ne error) error.addSuppressed(closeError) }
        throw error
    }
  }
}
