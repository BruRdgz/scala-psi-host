package scalapsihost

import com.intellij.codeInsight.daemon.impl.{AnnotationHolderImpl, AnnotationSessionImpl}
import com.intellij.core.{CoreApplicationEnvironment, CoreFileTypeRegistry}
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiErrorElement, PsiFile, SyntaxTraverser}
import com.intellij.psi.codeStyle.FileIndentOptionsProvider
import com.intellij.psi.impl.source.tree.TreeCopyHandler
import com.intellij.lang.{DefaultASTFactory, DefaultASTFactoryImpl, LanguageASTFactory, LanguageExtensionPoint, LanguageParserDefinitions, ParserDefinition, PsiBuilderFactory}
import com.intellij.lang.impl.PsiBuilderFactoryImpl
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.{Application, ApplicationManager, ModalityState}
import com.intellij.openapi.extensions.{DefaultPluginDescriptor, Extensions, PluginId}
import com.intellij.openapi.extensions.impl.{ExtensionPointImpl, ExtensionsAreaImpl}
import com.intellij.openapi.fileTypes.FileTypeRegistry
import com.intellij.openapi.module.Module
import com.intellij.openapi.progress.{EmptyProgressIndicator, ProgressManager}
import com.intellij.openapi.progress.impl.CoreProgressManager
import com.intellij.openapi.project.{DumbModeTask, DumbService, Project}
import com.intellij.openapi.util.{Disposer, Key, UserDataHolderBase}
import com.intellij.openapi.util.registry.Registry
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{FileViewProvider, LanguageFileViewProviders, PsiManager}
import com.intellij.psi.stubs.{LanguageStubDefinition, StubElementRegistryService, StubRegistryExtension}
import com.intellij.psi.util.PsiModificationTracker
import com.intellij.util.messages.impl.MessageBusFactoryImpl
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.project.{ProjectContext, ScalaFeatures}

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/**
 * @param stateDir        where the host writes to disk: the platform's state (index ID enumeration under
 *                        `idea.system.path`) and library files extracted for an editor to open
 * @param scalaPluginJars the Scala plugin's jars; they must also be on the classpath
 * @param unitTestMode    answer `isUnitTestMode` as the plugin's test fixture does; only for comparing with the
 *                        plugin's own tests, since the plugin changes behaviour in that mode
 * @param compilerErrorsScala3 errors of Scala 3 files come from the build's compiler, as by default in the IDE: the
 *                             plugin then reports only checks that do not need types (see [[ScalaPsiHost.isCompilerHighlighted]])
 * @param compilerErrorsScala2 the same for Scala 2 files (off by default, as in the IDE)
 */
final case class HostConfig(
  stateDir: Path,
  scalaPluginJars: Seq[Path],
  unitTestMode: Boolean = false,
  compilerErrorsScala3: Boolean = true,
  compilerErrorsScala2: Boolean = false,
)

/**
 * Scala PSI and type checker hosted without an IntelliJ application container. The host installs three fixed
 * tables — an `Application` object, a root extension area and a `Project` object — and then only consumes the
 * plugin's PSI. Whatever is not in a table fails loudly and is recorded in [[CallLog]].
 */
final class ScalaPsiHost private (
  val stateDir: Path,
  val project: Project,
  symbolIndex: SymbolIndex,
  libraryFiles: LibraryFileSystem,
  librarySources: ConcurrentHashMap[VirtualFile, VirtualFile],
  val modules: ModuleModel,
  psiChanged: () => Unit,
) {
  // Library jars by real path: a jar on the classpath of several modules is mounted and indexed once.
  private val libraryRoots = new ConcurrentHashMap[Path, VirtualFile]()
  // The mounted JDK's home (written in the write lock).
  @volatile private var jdkHome: Option[Path] = None

  /**
   * Runs `query` as the IDE's read actions run: concurrently with other queries, never during an edit. An edit that
   * starts meanwhile cancels it: it throws `ProcessCanceledException`. For callers that read PSI directly; the host's
   * own queries ([[diagnostics]]) do this themselves. Nested reads are part of the outer one.
   */
  def read[T](query: => T): T = HostLock.query(query)

  /** Cancels reads for an editor change already received but still waiting in the server's queue. */
  private[scalapsihost] def cancelReads(): Unit = HostLock.cancelQueries()

  /** Completion may preview insert handlers, so it enters the copy engine outside a semantic read. */
  def completion(file: ScalaFile, offset: Int, options: Completion.Options = Completion.Options()): org.eclipse.lsp4j.CompletionList =
    Completion.complete(file, offset, options = options)

  def resolveCompletion(file: ScalaFile, item: org.eclipse.lsp4j.CompletionItem, version: Int): org.eclipse.lsp4j.CompletionItem =
    Completion.resolve(file, item, version)

  /** Formatting returns edits against the current snapshot while leaving project PSI untouched. */
  def formatting(file: ScalaFile, range: Option[TextRange], options: org.eclipse.lsp4j.FormattingOptions): Seq[org.eclipse.lsp4j.TextEdit] =
    ChangeEngine.format(file, range, options).edits

  /** A file outside the project: parsed, but invisible to symbol lookups (as the IDE's non-physical files). */
  def parse(text: String, features: ScalaFeatures = ScalaFeatures.default): ScalaFile = HostLock.readLocked {
    ScalaPsiElementFactory.createScalaFileFromText(text, features)(using new ProjectContext(project))
  }

  /**
   * A project source file: its declarations are indexed, so other files and the file itself find them by name,
   * as for a file under an IDE source root. Fails when its stubs cannot be paired with its PSI.
   */
  def openSource(text: String, features: ScalaFeatures = ScalaFeatures.default): ScalaFile =
    HostLock.write(index(parse(text, features)))

  private def index[F <: PsiFile](file: F): F = {
    if (!symbolIndex.indexSourceFile(file)) {
      modules.removeSource(file.getViewProvider.getVirtualFile)
      throw new IllegalStateException("source file stubs could not be paired with its PSI")
    }
    psiChanged()
    file
  }

  /**
   * Opens a project source file of `module` (Scala or Java, by extension) with the given text: a physical file whose
   * PSI follows edits made with [[edit]], as a file open in an IDE editor.
   */
  def openFile(path: Path, text: String, module: HostModule): PsiFile = HostLock.write {
    val fileType = FileTypeRegistry.getInstance.getFileTypeByFileName(path.getFileName.toString)
    val source = new SourceFile(path, fileType, text)
    modules.addSource(source, module)
    val file = index(PsiManager.getInstance(project).findFile(source))
    // The document manager learns of document changes as a document listener (in the IDE, through the editor's
    // global event multicaster), so that a commit reparses what changed.
    val documents = com.intellij.psi.PsiDocumentManager.getInstance(project)
    documents match {
      case manager: HostPsiDocumentManager => manager.listenTo(documents.getDocument(file))
      case other => throw new IllegalStateException(s"unexpected document manager: ${other.getClass.getName}")
    }
    file
  }

  /**
   * Replaces `file`'s text between `start` and `end` (offsets) with `replacement`, as an editor change: the document
   * changes and is committed, the platform reparses the changed range, and the resulting tree changes reach the
   * plugin's caches. The file's index entries are then rebuilt. Queries running meanwhile are cancelled.
   */
  def edit(file: PsiFile, start: Int, end: Int, replacement: String): Unit = {
    HostLock.write {
      val documents = com.intellij.psi.PsiDocumentManager.getInstance(project)
      val document = documents.getDocument(file)
      if (document == null) throw new IllegalArgumentException(s"$file has no document: only files opened with openFile can be edited")
      document.replaceString(start, end, replacement)
      documents.commitDocument(document)
      symbolIndex.removeFile(file)
      if (!symbolIndex.indexSourceFile(file)) throw new IllegalStateException(s"stubs of the edited $file could not be paired with its PSI")
    }
    // Work the commit scheduled for the UI thread (after-commit actions) runs before the edit returns.
    HostDispatchThread.drain()
  }

  /** Removes a source file opened with [[openSource]] or [[openFile]] from the project. */
  def closeSource(file: PsiFile): Unit = HostLock.write {
    val virtualFile = file.getViewProvider.getVirtualFile
    symbolIndex.removeFile(file)
    modules.removeSource(virtualFile)
    // The file store lets go of its view provider (and PSI): a file opened again at the same path is a new file.
    PsiManager.getInstance(project).asInstanceOf[com.intellij.psi.impl.PsiManagerEx].getFileManager.setViewProvider(virtualFile, null)
    psiChanged()
  }

  /**
   * Removes a module whose source files were closed (after a build change); a module that depends on it is removed
   * too, or added again with it. Its libraries stay mounted, for other modules and for the module added again.
   */
  def removeModule(name: String): Unit = HostLock.write {
    modules.remove(name)
    psiChanged()
  }

  /** Mounts a library jar and indexes its Scala class files accepted by `includeEntry` (e.g. `scala/Option.class`). */
  def addLibrary(jar: Path, includeEntry: String => Boolean = _ => true): LibraryIndexReport = HostLock.write {
    val root = libraryFiles.mountJar(jar)
    libraryRoots.put(jar.toRealPath(), root)
    val report = symbolIndex.indexLibrary(project, root, includeEntry)
    psiChanged()
    report
  }

  /**
   * Attaches the sources of a mounted library jar (a source jar), as the IDE attaches library sources: navigation
   * from the jar's classes leads to them. Source files are parsed when navigated to, never indexed.
   */
  def addLibrarySources(jar: Path, sources: Path): Unit = HostLock.write {
    val root = Option(libraryRoots.get(jar.toRealPath())).getOrElse(throw new IllegalArgumentException(s"library jar is not mounted: $jar"))
    librarySources.put(root, libraryFiles.mountSources(sources))
    psiChanged()
  }

  /** Attaches the JDK's sources (its `lib/src.zip`, one directory per module) to the mounted JDK's modules. */
  def addJdkSources(srcZip: Path): Unit = HostLock.write {
    for (moduleRoot <- modules.jdkModuleRoots) {
      val module = moduleRoot.getName.substring(moduleRoot.getName.lastIndexOf("!/") + 2)
      val sources = libraryFiles.mountSources(srcZip, module)
      // A JDK image may ship sources for some of its modules only.
      if (sources.getChildren.nonEmpty) librarySources.put(moduleRoot, sources)
    }
    psiChanged()
  }

  /**
   * Mounts the JDK's modules. Java classes are read on demand, not indexed. Every module sees the JDK. Mounting the
   * JDK that is already mounted does nothing.
   */
  def addJdk(javaHome: Path): Unit = HostLock.write {
    if (!jdkHome.contains(javaHome.toRealPath())) {
      if (jdkHome.nonEmpty) throw new IllegalStateException(s"a JDK is already mounted (${jdkHome.get}); the host has one JDK")
      modules.setJdkRoots(libraryFiles.mountJdk(javaHome))
      jdkHome = Some(javaHome.toRealPath())
      psiChanged()
    }
  }

  /**
   * Adds a module as an IDE module after a build import: the jars of its classpath are mounted and indexed (each
   * jar once, however many modules use it), its Scala version becomes a Scala SDK library and its compiler options
   * a compiler-settings profile, from which the plugin derives the module's language features.
   * Classpath entries must be jars; the class directories of other modules are given by `dependsOn`.
   */
  def addModule(spec: ModuleSpec): HostModule = HostLock.write {
    val notJars = spec.classpath.filterNot(_.getFileName.toString.endsWith(".jar"))
    if (notJars.nonEmpty) throw new IllegalArgumentException(s"module ${spec.name}: classpath entries are not jars: ${notJars.mkString(", ")}")
    val roots = spec.classpath.map { jar =>
      Option(libraryRoots.get(jar.toRealPath())).getOrElse { addLibrary(jar); libraryRoots.get(jar.toRealPath()) }
    }
    val hostModule = new HostModule(spec, roots.toSet)
    hostModule.module = newModule(hostModule)
    modules.add(hostModule)
    org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration.instanceIn(project).configureSettingsForModule(
      spec.name, "bsp",
      org.jetbrains.plugins.scala.project.settings.ScalaCompilerSettings.fromOptions(
        spec.scalacOptions, org.jetbrains.plugins.scala.compiler.data.CompileOrder.Mixed))
    org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration.incModificationCount()
    psiChanged()
    hostModule
  }

  /**
   * The IDE `Module` object of a host module. Its only order entry the plugin reads is the Scala SDK library, which
   * carries the module's Scala version; the classpath jars are not presented as libraries (scopes come from
   * [[ModuleModel]]).
   */
  private def newModule(hostModule: HostModule): Module = {
    val data = new UserDataHolderBase
    val sdkProperties = org.jetbrains.plugins.scala.project.ScalaLibraryProperties(
      Some(hostModule.spec.scalaVersion), Seq.empty, Seq.empty, None, org.jetbrains.plugins.scala.project.ReplClasspath.Bundled)
    lazy val scalaSdk: com.intellij.openapi.roots.impl.libraries.LibraryEx =
      tableProxy(classOf[com.intellij.openapi.roots.impl.libraries.LibraryEx], "ScalaSdkLibrary") {
        // ScalaModuleSettings reads the compiler version from the library name, as for an imported "scala-sdk-x.y.z".
        case ("getName", _) => s"scala-sdk-${hostModule.spec.scalaVersion}"
        case ("getKind", _) => org.jetbrains.plugins.scala.project.ScalaLibraryType.Kind
        case ("getProperties", _) => sdkProperties
        case ("isDisposed", _) => java.lang.Boolean.FALSE
      }
    lazy val orderEntries: com.intellij.openapi.roots.OrderEnumerator = new Host_OrderEnumerator(dispatch {
      // Filters that keep libraries leave the one entry in place.
      case ("OrderEnumerator.withoutSdk" | "OrderEnumerator.withoutDepModules" | "OrderEnumerator.withoutModuleSourceEntries"
            | "OrderEnumerator.recursively" | "OrderEnumerator.exportedOnly" | "OrderEnumerator.compileOnly"
            | "OrderEnumerator.productionOnly", _) => orderEntries
      case ("OrderEnumerator.forEachLibrary", Array(p: com.intellij.util.Processor[?])) =>
        p.asInstanceOf[com.intellij.util.Processor[com.intellij.openapi.roots.libraries.Library]].process(scalaSdk); null
    })
    lazy val rootManager: com.intellij.openapi.roots.ModuleRootManager = new Host_ModuleRootManager(dispatch {
      case ("ModuleRootManager.orderEntries", _) => orderEntries
      case ("ModuleRootManager.getModule", _) => module
      // Host modules declare source membership, but no content entries or inherited package prefixes.
      case ("ModuleRootManager.getContentEntries", _) => Array.empty[com.intellij.openapi.roots.ContentEntry]
    })
    lazy val module: Module = tableProxy(classOf[Module], "Module") {
      case ("getName", _) => hostModule.spec.name
      case ("getProject", _) => project
      case ("isDisposed", _) => java.lang.Boolean.FALSE
      case ("getModuleTypeName", _) => "JAVA_MODULE"
      case ("getService", Array(c: Class[?])) if c == classOf[com.intellij.openapi.roots.ModuleRootManager] => rootManager
      case ("getUserData", Array(k: Key[?])) => data.getUserData(k).asInstanceOf[AnyRef]
      case ("putUserData", Array(k: Key[?], v)) => data.putUserData(k.asInstanceOf[Key[AnyRef]], v); null
    }
    module
  }

  /**
   * Whether `file`'s errors come from the build's compiler (see [[HostConfig.compilerErrorsScala3]]). For such a file
   * [[diagnostics]] has only the plugin's checks that do not need types, and the caller adds the compiler's problems.
   */
  def isCompilerHighlighted(file: ScalaFile): Boolean = HostLock.query {
    org.jetbrains.plugins.scala.settings.ScalaHighlightingMode.isShowErrorsFromCompilerEnabled(file)
  }

  /**
   * The problems the IDE's highlighting pass reports for `file`: syntax errors, and the plugin's `ScalaAnnotator`
   * run over every element through the platform's annotation session. Presentation-only annotations (no message)
   * are left out.
   */
  def diagnostics(file: ScalaFile): Seq[Diagnostic] = HostLock.query {
    // As the IDE's highlighting visitor: error elements the filters reject (e.g. inside ScalaDoc) are not reported.
    val errorFilters = com.intellij.codeInsight.highlighting.HighlightErrorFilter.EP_NAME.getExtensions(project).asScala
    val syntaxErrors = SyntaxTraverser.psiTraverser(file).filter(classOf[PsiErrorElement]).asScala.toSeq
      .filter(e => errorFilters.forall(_.shouldHighlightErrorElement(e)))
      .map(e => Diagnostic(HighlightSeverity.ERROR, e.getTextRange, e.getErrorDescription))
    val annotator = new org.jetbrains.plugins.scala.annotator.ScalaAnnotator
    val annotations = AnnotationSessionImpl.computeWithSession(file, /* batchMode = */ false, annotator, { holder =>
      val impl = holder.asInstanceOf[AnnotationHolderImpl]
      SyntaxTraverser.psiTraverser(file).forEach(e => impl.runAnnotatorWithContext(e))
      impl.asScala.toSeq
    })
    syntaxErrors ++ annotations.collect {
      case a if a.getMessage != null =>
        Diagnostic(a.getSeverity, new TextRange(a.getStartOffset, a.getEndOffset), a.getMessage)
    }
  }

  /**
   * The colors the IDE gives `file`'s identifiers and soft keywords beyond its lexer's: the plugin's syntax highlighting
   * visitor (declarations, parameters, named arguments, annotations) and its color-scheme annotator (references, by
   * what they resolve to), in that order, as the IDE's highlighting pass runs them.
   */
  def textAttributes(file: ScalaFile): Seq[(TextRange, com.intellij.openapi.editor.colors.TextAttributesKey)] = HostLock.query {
    val elements = SyntaxTraverser.psiTraverser(file).asScala.toSeq
    val holder = new com.intellij.codeInsight.daemon.impl.analysis.HighlightInfoHolder(file)
    val visitor = new org.jetbrains.plugins.scala.highlighter.ScalaSyntaxHighlightingVisitor
    visitor.analyze(file, /* updateWholeFile = */ true, holder, () => elements.foreach(visitor.visit))
    val fromVisitor = (0 until holder.size).map(holder.get).flatMap { info =>
      Option(info.forcedTextAttributesKey).orElse(Option(info.`type`.getAttributesKey))
        .map(new TextRange(info.getStartOffset, info.getEndOffset) -> _)
    }
    val annotator = new org.jetbrains.plugins.scala.highlighter.ScalaColorSchemeAnnotator
    val annotations = AnnotationSessionImpl.computeWithSession(file, /* batchMode = */ false, annotator, { holder =>
      val impl = holder.asInstanceOf[AnnotationHolderImpl]
      elements.foreach(e => impl.runAnnotatorWithContext(e))
      impl.asScala.toSeq
    })
    fromVisitor ++ annotations.flatMap(a => Option(a.getTextAttributes).map(new TextRange(a.getStartOffset, a.getEndOffset) -> _))
  }
}

final case class Diagnostic(severity: HighlightSeverity, range: TextRange, message: String)

object ScalaPsiHost {
  @volatile private var started: Option[ScalaPsiHost] = None

  /** Plugin extension points that carry language semantics; every other plugin extension point stays empty. */
  val SemanticExtensionPoints: Seq[String] = Seq("org.intellij.scala.syntheticMemberInjector")

  /** Initialization, visible symbols, syntax and literal values; imports and generated edits need insert handlers. */
  val CompletionContributors: Set[String] = Set(
    "ScalaDumbAwareCompletionContributor", "ScalaBasicCompletionContributor",
    "ScalaKeywordCompletionContributor", "ScalaLiteralTypeValuesCompletionContributor",
  ).map("org.jetbrains.plugins.scala.lang.completion." + _)

  /** The plugin's completion and proximity weighers, used with their declared keys and ordering. */
  val CompletionWeighers: Set[String] = Set(
    "ScalaContainingClassWeigher", "ScalaMethodCompletionWeigher", "ScalaByNameWeigher", "ScalaKindCompletionWeigher",
    "ScalaScopeWeigher", "ScalaClassObjectWeigher", "ScalaExplicitlyImportedWeigher",
  ).map("org.jetbrains.plugins.scala.lang.completion.weighter." + _)

  /** Platform ordering anchors from the matching analysis-impl/lang-impl descriptors; statistics stay inert in BaseCompletionService. */
  private val PlatformCompletionWeighers: Seq[ExtensionBean] = Seq(
    ("StatisticsWeigher", "stats", "after prefix"), ("PriorityWeigher", "priority", "first"),
    ("PrefixMatchingWeigher", "prefix", "after priority"), ("GroupingWeigher", "grouping", "last"),
    ("ExplicitProximityWeigher", "explicitProximity", "after stats"), ("LookupElementProximityWeigher", "proximity", "after explicitProximity"),
  ).map { (name, id, order) => ExtensionBean(Map("key" -> "completion", "id" -> id, "order" -> order,
    "implementationClass" -> s"com.intellij.codeInsight.completion.$name")) }

  /**
   * Starts the host. The platform keeps the Application and the root extension area in process-wide statics,
   * so there is one host per JVM and it lives until the JVM exits.
   */
  def start(config: HostConfig): ScalaPsiHost = synchronized {
    if (started.isDefined) throw new IllegalStateException("ScalaPsiHost is already started in this JVM")
    val host = bootstrap(config)
    started = Some(host)
    host
  }

  private def bootstrap(config: HostConfig): ScalaPsiHost = {
    // PathManager reads these on first use; the ID enumeration of index keys is persisted under the system path.
    for (kind <- Seq("home", "system", "config", "log")) {
      val dir = config.stateDir.resolve(kind)
      Files.createDirectories(dir)
      System.setProperty(s"idea.$kind.path", dir.toString)
    }
    com.intellij.openapi.diagnostic.Logger.setFactory((category: String) => new HostLogger(category))
    val root: Disposable = Disposer.newDisposable("scala-psi-host")
    val appServices = new ServiceTable("Application")
    val projectServices = new ServiceTable("Project")
    val appData = new UserDataHolderBase
    val projectData = new UserDataHolderBase

    // ---- 1. Application object -------------------------------------------------------------
    // An inert in-process bus: platform classes subscribe in constructors; the host never publishes IDE events.
    val busOwner = tableProxy(classOf[com.intellij.util.messages.MessageBusOwner], "MessageBusOwner") {
      case ("isDisposed", _) => java.lang.Boolean.FALSE
    }
    lazy val appBus = MessageBusFactoryImpl.createRootBus(busOwner)
    lazy val area: ExtensionsAreaImpl = new ExtensionsAreaImpl(app)
    // ApplicationEx, the interface the IDE's application implements: platform utilities cast to it.
    lazy val app: Application = tableProxy(classOf[com.intellij.openapi.application.ex.ApplicationEx], "Application") {
      case ("getService", Array(c: Class[?])) => appServices.get(c)
      case ("getServiceIfCreated", Array(c: Class[?])) => appServices.get(c)
      case ("getExtensionArea", _) => area
      case ("getMessageBus", _) => appBus
      // UI dispatch and access ownership are supplied by the host's threads and lock.
      case ("isDispatchThread", _) => java.lang.Boolean.valueOf(HostDispatchThread.isCurrent)
      case ("isWriteIntentLockAcquired", _) => java.lang.Boolean.valueOf(HostLock.hasWriteIntent)
      case ("runWriteIntentReadAction", Array(c: com.intellij.openapi.util.ThrowableComputable[?, ?])) =>
        HostLock.writeIntent(c.compute()).asInstanceOf[AnyRef]
      case ("addReadActionListener", Array(listener: com.intellij.openapi.application.ReadActionListener, parent: Disposable)) =>
        HostLock.addReadListener(listener, parent); null
      case ("addWriteIntentReadActionListener", Array(listener: com.intellij.openapi.application.WriteIntentReadActionListener, parent: Disposable)) =>
        HostLock.addIntentListener(listener, parent); null
      case ("addWriteActionListener", Array(listener: com.intellij.openapi.application.WriteActionListener, parent: Disposable)) =>
        HostLock.addWriteListener(listener, parent); null
      // Reads and writes are the host's lock (HostLock): access is allowed where the thread holds it.
      case ("isReadAccessAllowed", _) => java.lang.Boolean.valueOf(HostLock.isReading || HostLock.hasWriteIntent)
      case ("assertReadAccessAllowed", _) =>
        if (!HostLock.isReading && !HostLock.hasWriteIntent) throw new IllegalStateException("read access is allowed inside read or write actions only")
        null
      case ("isWriteAccessAllowed", _) => java.lang.Boolean.valueOf(HostLock.isWriting)
      // Work for the UI thread runs on the host's dispatch thread.
      case ("invokeLater", Array(r: Runnable, _*)) => HostDispatchThread.invokeLater(r); null
      // No modal dialogs exist in the host.
      case ("getDefaultModalityState", _) => ModalityState.nonModal()
      case ("assertWriteAccessAllowed", _) =>
        if (!HostLock.isWriting) throw new IllegalStateException("write access is allowed inside write actions only")
        null
      case ("runWriteAction", Array(r: Runnable)) => HostLock.write(r.run()); null
      case ("runWriteAction", Array(c: com.intellij.openapi.util.Computable[?])) => HostLock.write(c.compute()).asInstanceOf[AnyRef]
      case ("runWriteAction", Array(c: com.intellij.openapi.util.ThrowableComputable[?, ?])) => HostLock.write(c.compute()).asInstanceOf[AnyRef]
      case ("assertIsNonDispatchThread" | "assertReadAccessNotAllowed", _) => null
      case ("isUnitTestMode", _) => java.lang.Boolean.valueOf(config.unitTestMode)
      case ("isInternal" | "isEAP" | "isDisposed", _) => java.lang.Boolean.FALSE
      case ("isHeadlessEnvironment" | "isCommandLine", _) => java.lang.Boolean.TRUE
      case ("getUserData", Array(k: Key[?])) => appData.getUserData(k).asInstanceOf[AnyRef]
      // Extension points resolve their declared class through the component manager.
      case ("loadClass", Array(name: String, _)) => Class.forName(name)
      case ("isInjectionForExtensionSupported", _) => java.lang.Boolean.FALSE
      // Completion bean extensions use the platform's lazy loader, without container injection.
      case ("instantiateClass", Array(cls: Class[?], _)) if
          ScalaPsiHost.CompletionContributors.contains(cls.getName) ||
          ScalaPsiHost.CompletionWeighers.contains(cls.getName) ||
          Set("com.intellij.codeInsight.completion.CompletionContributorEP", "com.intellij.psi.WeigherExtensionPoint").contains(cls.getName) =>
        newPrivate(cls).asInstanceOf[AnyRef]
      case ("instantiateClass", Array(name: String, _)) if
          ScalaPsiHost.CompletionContributors(name) || ScalaPsiHost.CompletionWeighers(name) ||
          PlatformCompletionWeighers.exists(_.implementation == name) =>
        newPrivate(Class.forName(name))
      // Wraps an extension's failure for logging; the IDE adds plugin attribution, which the host does not track.
      case ("createError", Array(t: Throwable, _*)) => new RuntimeException(t)
      case ("runReadAction", Array(r: Runnable)) => HostLock.readLocked(r.run()); null
      case ("tryRunReadAction", Array(r: Runnable)) => HostLock.readLocked(r.run()); java.lang.Boolean.TRUE
      // In unit-test mode the plugin runs some startup work on the UI thread: the host's dispatch thread.
      case ("invokeAndWait", Array(r: Runnable, _*)) if config.unitTestMode => HostDispatchThread.invokeAndWait(r); null
      case ("runReadAction", Array(c: com.intellij.openapi.util.Computable[?])) => HostLock.readLocked(c.compute()).asInstanceOf[AnyRef]
      case ("runReadAction", Array(c: com.intellij.openapi.util.ThrowableComputable[?, ?])) => HostLock.readLocked(c.compute()).asInstanceOf[AnyRef]
    }
    ApplicationManager.setApplication(app, root)
    Extensions.setRootArea(area)

    val classpathJars = System.getProperty("java.class.path").split(File.pathSeparator).toSeq
      .filter(_.endsWith(".jar")).map(Path.of(_))
    val epDecls = Descriptors.extensionPoints(classpathJars, config.scalaPluginJars)
    def declare(name: String): ExtensionPointImpl[?] = {
      val d = epDecls.getOrElse(name, throw new IllegalStateException(s"extension point is not declared on the classpath: $name"))
      area.registerExtensionPoint(d.name, d.className, d.kind, d.dynamic)
      area.getExtensionPoint(name).asInstanceOf[ExtensionPointImpl[?]]
    }
    // The plugin's own extension points, with no extensions.
    val pluginEps = epDecls.values.filter(_.fromScalaPlugin)
    if (pluginEps.isEmpty)
      throw new IllegalStateException(s"no extension points declared by the Scala plugin jars ${config.scalaPluginJars.mkString(", ")}; are they on the classpath?")
    pluginEps.foreach(d => declare(d.name))
    // The plugin's extension points whose extensions are language semantics, loaded as its descriptors register
    // them. syntheticMemberInjector: case-class and companion members, context bounds, enums, `derives`, and the
    // members library macros generate (Monocle, Circe, Simulacrum, ...), as the IDE always runs them.
    for (epName <- ScalaPsiHost.SemanticExtensionPoints) {
      val implementations = Descriptors.scalaExtensions(config.scalaPluginJars, epName)
      if (implementations.isEmpty) throw new IllegalStateException(s"no extensions registered for $epName in the Scala plugin's descriptors")
      val ep = area.getExtensionPoint[AnyRef](epName).asInstanceOf[ExtensionPointImpl[AnyRef]]
      implementations.foreach(name => ep.registerExtension(newPrivate(Class.forName(name).asInstanceOf[Class[AnyRef]]), root))
    }
    // Descriptor beans preserve both the implementation attributes and the platform's id/order semantics.
    val completionPlugin = new DefaultPluginDescriptor(PluginId.getId("org.intellij.scala"))
    def registerCompletionBeans(epName: String, beans: Seq[ExtensionBean], plugin: com.intellij.openapi.extensions.PluginDescriptor): Unit = {
      val ep = area.getExtensionPoint(epName).asInstanceOf[ExtensionPointImpl[?]]
      val descriptors = beans.map { bean =>
        val attributes = bean.attributes -- (if (epName.endsWith("weigher")) Seq("order") else Seq("id", "order"))
        new com.intellij.openapi.extensions.ExtensionDescriptor(null, null, bean.attributes.getOrElse("id", null),
          com.intellij.openapi.extensions.LoadingOrder.readOrder(bean.attributes.getOrElse("order", "")),
          new com.intellij.util.xml.dom.XmlElement(epName.stripPrefix("com.intellij."), attributes.asJava, java.util.List.of(), null), true)
      }
      ep.registerExtensions(descriptors.asJava, plugin, java.util.List.of())
    }
    for (epName <- Seq("com.intellij.completion.contributor", "com.intellij.weigher")) {
      declare(epName)
      val beans = Descriptors.scalaExtensionBeans(config.scalaPluginJars, epName).filter { bean =>
        if (epName.endsWith("contributor"))
          bean.attributes.get("language").contains("Scala") && ScalaPsiHost.CompletionContributors(bean.implementation)
        else ScalaPsiHost.CompletionWeighers(bean.implementation) && Set("completion", "proximity").contains(bean.attributes.getOrElse("key", ""))
      }
      if (beans.isEmpty) throw new IllegalStateException(s"no completion beans for $epName")
      registerCompletionBeans(epName, beans, completionPlugin)
    }
    registerCompletionBeans("com.intellij.weigher", PlatformCompletionWeighers, new DefaultPluginDescriptor(PluginId.getId("com.intellij")))
    for (name <- Seq("com.intellij.modcompletion.completionItemProvider", "com.intellij.modcompletion.completionItemFilter"))
      CoreApplicationEnvironment.registerExtensionPoint(area, name, classOf[LanguageExtensionPoint[?]])
    // These popup requests are optional UI hints; recording intent leaves the synchronous insertion valid without a popup.
    projectServices.register(classOf[com.intellij.codeInsight.AutoPopupController])(new com.intellij.codeInsight.AutoPopupController {
      override def autoPopupParameterInfo(editor: com.intellij.openapi.editor.Editor, element: com.intellij.psi.PsiElement): Unit =
        CallLog.hit("Completion.popup.parameterInfo")
      override def scheduleAutoPopup(editor: com.intellij.openapi.editor.Editor, kind: com.intellij.codeInsight.completion.CompletionType,
                                    condition: com.intellij.openapi.util.Condition[? >: PsiFile]): Unit =
        CallLog.hit("Completion.popup.completion")
      override def autoPopupMemberLookup(editor: com.intellij.openapi.editor.Editor,
                                        condition: com.intellij.openapi.util.Condition[? >: PsiFile]): Unit =
        scheduleAutoPopup(editor, com.intellij.codeInsight.completion.CompletionType.BASIC, condition)
      override def autoPopupMemberLookup(editor: com.intellij.openapi.editor.Editor, kind: com.intellij.codeInsight.completion.CompletionType,
                                        condition: com.intellij.openapi.util.Condition[? >: PsiFile]): Unit =
        scheduleAutoPopup(editor, kind, condition)
      override def cancelAllRequests(): Unit = CallLog.unsupported("AutoPopupController.cancelAllRequests")
      override def waitForDelayedActions(timeout: Long, unit: java.util.concurrent.TimeUnit): Unit =
        CallLog.unsupported("AutoPopupController.waitForDelayedActions")
    })
    appServices.register(classOf[com.intellij.codeInsight.completion.CompletionService])(new Completion.Service)
    appServices.register(classOf[com.intellij.codeInsight.CodeInsightSettings])(new com.intellij.codeInsight.CodeInsightSettings)
    // Lookup presentation checks test-class icons; no test-framework integrations are installed in the host.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.testIntegration.TestFramework.EXTENSION_NAME,
      classOf[com.intellij.testIntegration.TestFramework])
    CoreApplicationEnvironment.registerExtensionPoint(area, "com.intellij.javaMainMethodProvider",
      classOf[com.intellij.codeInsight.runner.JavaMainMethodProvider])
    CoreApplicationEnvironment.registerExtensionPoint(area, "com.intellij.packageDirectoryProvider",
      classOf[com.intellij.psi.PackageDirectoryProvider])
    appServices.register(classOf[com.intellij.codeInsight.TestFrameworks])(newPrivate(classOf[com.intellij.codeInsight.TestFrameworksImpl]))
    appServices.registerNull(classOf[com.intellij.codeInsight.completion.NewRdCompletionSupport])
    appServices.register(classOf[com.intellij.codeInsight.completion.group.GroupedCompletion])(
      tableProxy(classOf[com.intellij.codeInsight.completion.group.GroupedCompletion], "GroupedCompletion") {
        case ("isEnabled", _) => java.lang.Boolean.FALSE
      })
    // Platform extension points the plugin's PSI queries, with no extensions. Their declarations live in IDE
    // descriptors that are not on this classpath; the platform's own core environment declares them in code too.
    CoreApplicationEnvironment.registerExtensionPoint(area, FileIndentOptionsProvider.EP_NAME, classOf[FileIndentOptionsProvider])
    CoreApplicationEnvironment.registerExtensionPoint(area, TreeCopyHandler.EP_NAME, classOf[TreeCopyHandler])
    CoreApplicationEnvironment.registerExtensionPoint(area, "com.intellij.multiverseEnabler", classOf[com.intellij.codeInsight.multiverse.MultiverseEnabler])
    // Java PSI of library classes asks for augmentations (Lombok-style); there are none.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.psi.augment.PsiAugmentProvider.EP_NAME, classOf[com.intellij.psi.augment.PsiAugmentProvider])
    // Which syntax errors are shown: not inside ScalaDoc, and not in files whose errors come from the compiler.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.codeInsight.highlighting.HighlightErrorFilter.EP_NAME.getName,
      classOf[com.intellij.codeInsight.highlighting.HighlightErrorFilter])
    locally {
      val filters = area.getExtensionPoint[com.intellij.codeInsight.highlighting.HighlightErrorFilter](
        com.intellij.codeInsight.highlighting.HighlightErrorFilter.EP_NAME.getName)
      filters.registerExtension(new org.jetbrains.plugins.scala.codeInsight.highlighting.ScalaDocHighlightErrorFilter, root)
      // The plugin's CompilerHighlightingErrorFilter lives in its compiler-integration module (JPS, compile server),
      // which the host does not load; this is the same rule, decided by the plugin's ScalaHighlightingMode.
      filters.registerExtension(new com.intellij.codeInsight.highlighting.HighlightErrorFilter {
        override def shouldHighlightErrorElement(element: com.intellij.psi.PsiErrorElement): Boolean =
          !org.jetbrains.plugins.scala.settings.ScalaHighlightingMode.isShowErrorsFromCompilerEnabled(element.getContainingFile)
      }, root)
    }
    // Custom folding regions in comments (`//region` ... `//endregion`, `// <editor-fold>`): the IDE's two providers.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.lang.folding.CustomFoldingProvider.EP_NAME,
      classOf[com.intellij.lang.folding.CustomFoldingProvider])
    locally {
      val providers = area.getExtensionPoint[com.intellij.lang.folding.CustomFoldingProvider](com.intellij.lang.folding.CustomFoldingProvider.EP_NAME.getName)
      providers.registerExtension(new com.intellij.lang.customFolding.NetBeansCustomFoldingProvider, root)
      providers.registerExtension(new com.intellij.lang.customFolding.VisualStudioCustomFoldingProvider, root)
    }
    // Filters of highlighting results (the IDE's hide some in debugger and diff views); the plugin registers none.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.codeInsight.daemon.impl.HighlightInfoFilter.EXTENSION_POINT_NAME,
      classOf[com.intellij.codeInsight.daemon.impl.HighlightInfoFilter])
    // Languages that stand for groups of languages in language extensions; none in the host.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.lang.MetaLanguage.EP_NAME, classOf[com.intellij.lang.MetaLanguage])
    // Smart pointers' anchors and element icons: no providers in the host.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.psi.impl.smartPointers.SmartPointerAnchorProvider.EP_NAME,
      classOf[com.intellij.psi.impl.smartPointers.SmartPointerAnchorProvider])
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.ide.IconProvider.EXTENSION_POINT_NAME, classOf[com.intellij.ide.IconProvider])
    // Java class files look for their source mirror through these policies; the host attaches no library sources.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.psi.impl.compiled.ClsCustomNavigationPolicy.EP_NAME,
      classOf[com.intellij.psi.impl.compiled.ClsCustomNavigationPolicy])

    appServices.register(classOf[ProgressManager])(new CoreProgressManager)
    appServices.register(classOf[PsiBuilderFactory])(new PsiBuilderFactoryImpl)
    // The builder of the platform's newer syntax layer, which the Java parser uses.
    appServices.register(classOf[com.intellij.platform.syntax.psi.PsiSyntaxBuilderFactory])(
      new com.intellij.platform.syntax.psi.PsiSyntaxBuilderFactoryImpl)
    appServices.register(classOf[DefaultASTFactory])(new DefaultASTFactoryImpl)
    // In-memory documents; in the LSP host these become the client's snapshots.
    appServices.register(classOf[com.intellij.openapi.fileEditor.FileDocumentManager])(
      // Documents for use off the UI thread (the platform's option for background documents): the host has no UI
      // thread, and its write action keeps writers apart.
      new com.intellij.mock.MockFileDocumentManagerImpl(null, (text: CharSequence) =>
        new com.intellij.openapi.editor.impl.DocumentImpl(text, /* forUseInNonAWTThread = */ true)))
    // Library source files are read as the IDE reads files from disk, guessing their charset: UTF-8 by default (the
    // JDK's default since 18, and the build tools'), none set per file.
    appServices.register(classOf[com.intellij.openapi.vfs.encoding.EncodingManager])(new Host_EncodingManager(dispatch {
      case ("EncodingManager.getEncoding", _) => null
      case ("EncodingManager.getDefaultCharset", _) => java.nio.charset.StandardCharsets.UTF_8
    }))
    // Transformations between a file's bytes and its text presentation, by file type; none for Scala or Java.
    declare(com.intellij.openapi.vfs.transformer.TextPresentationTransformers.Companion.getEP.getName)
    appServices.register(classOf[com.intellij.openapi.vfs.transformer.TextPresentationTransformers])(
      new com.intellij.openapi.vfs.transformer.TextPresentationTransformers)
    // PSI writes check command state; the core processor tracks it without UI or undo.
    appServices.register(classOf[com.intellij.openapi.command.CommandProcessor])(new com.intellij.openapi.command.impl.CoreCommandProcessor)
    // PSI versioning (MVCC): the platform's service, which with persistent syntax trees off (the registry default)
    // creates plain PSI elements during reparses.
    appServices.register(classOf[com.intellij.psi.util.PsiVersioningService])(new com.intellij.psi.impl.source.tree.mvcc.PsiVersioningServiceImpl)
    // "Run on the UI thread, carrying the write action": the host has no UI thread and the caller already holds the
    // write action, so the action runs where it is.
    appServices.register(classOf[com.intellij.util.concurrency.TransferredWriteActionService])(
      tableProxy(classOf[com.intellij.util.concurrency.TransferredWriteActionService], "TransferredWriteActionService") {
        case ("runOnEdtWithTransferredWriteActionAndWait" | "runOnBackgroundThreadWithTransferredWriteActionAndWait", Array(r: Runnable)) =>
          r.run(); null
      })
    // Document commits: synchronous, on the editing thread.
    appServices.register(classOf[com.intellij.psi.impl.DocumentCommitProcessor])(new HostDocumentCommitProcessor)
    // Read through serviceOrNull: PSI MVCC is off by default (registry psi.enable.persistent.syntax.tree=false).
    appServices.registerNull(classOf[com.intellij.psi.impl.source.tree.mvcc.PsiVersioningGarbageCollector])
    // Optional diagnostics hook; the caller null-checks it.
    appServices.registerNull(classOf[com.intellij.psi.ParsingDiagnostics.ParserDiagnosticsHandler])
    appServices.register(classOf[com.intellij.openapi.application.AsyncExecutionService])(new HostAsyncExecutionService)
    // Private constructors: the IDE container instantiates these reflectively, and so does the host.
    appServices.register(classOf[com.intellij.psi.LanguageSubstitutors])(newPrivate(classOf[com.intellij.psi.LanguageSubstitutors]))
    // Holds one flag (external change in progress) that PSI events read.
    locally {
      val cls = Class.forName("com.intellij.psi.ExternalChangeActionUtil$PsiExternalChangeService").asInstanceOf[Class[AnyRef]]
      appServices.register(cls)(newPrivate(cls))
    }
    // The plugin's settings provider links UI classes absent from this classpath. Its customizeDefaults sets
    // these three indent sizes to 2 and KEEP_FIRST_COLUMN_COMMENT to false; the settings bean owns import defaults.
    val scalaStyleProvider = tableProxy(classOf[com.intellij.psi.codeStyle.LanguageCodeStyleProvider], "ScalaCodeStyleProvider") {
      case ("getLanguage", _) => org.jetbrains.plugins.scala.ScalaLanguage.INSTANCE
      case ("getDefaultCommonSettings", _) =>
        val common = new com.intellij.psi.codeStyle.CommonCodeStyleSettings(org.jetbrains.plugins.scala.ScalaLanguage.INSTANCE)
        val indent = common.initIndentOptions()
        val ScalaDefaultIndentSize = 2
        indent.INDENT_SIZE = ScalaDefaultIndentSize
        indent.TAB_SIZE = ScalaDefaultIndentSize
        indent.CONTINUATION_INDENT_SIZE = ScalaDefaultIndentSize
        common.KEEP_FIRST_COLUMN_COMMENT = false
        common
      case ("getDocCommentSettings", _) => com.intellij.psi.codeStyle.DocCommentSettings.DEFAULTS
      case ("createCustomSettings", Array(settings: com.intellij.psi.codeStyle.CodeStyleSettings)) =>
        new org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings(settings)
    }
    appServices.register(classOf[com.intellij.psi.codeStyle.CodeStyleSettingsService])(
      tableProxy(classOf[com.intellij.psi.codeStyle.CodeStyleSettingsService], "CodeStyleSettingsService") {
        case ("addListener", _) => null
        case ("getFileTypeIndentOptionsFactories", _) => java.util.List.of()
        case ("getLanguageCodeStyleProviders", _) =>
          java.util.List.of(scalaStyleProvider)
        // Only Scala's custom settings bean, with defaults; not the settings-UI provider.
        case ("getCustomCodeStyleSettingsFactories", _) =>
          java.util.List.of(new com.intellij.psi.codeStyle.CustomCodeStyleSettingsFactory {
            override def createCustomSettings(s: com.intellij.psi.codeStyle.CodeStyleSettings) =
              new org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings(s)
          })
      })
    appServices.register(classOf[com.intellij.formatting.Formatter])(new com.intellij.formatting.FormatterImpl)
    appServices.register(classOf[com.intellij.formatting.FormattingProgressCallbackFactory])(
      tableProxy(classOf[com.intellij.formatting.FormattingProgressCallbackFactory], "FormattingProgressCallbackFactory") {
        // The optional UI callback is absent; cancellation uses the request's progress indicator.
        case ("createProgressCallback", _) => null
      })
    CoreApplicationEnvironment.registerExtensionPoint(area, "com.intellij.lang.formatter", classOf[LanguageExtensionPoint[?]])
    com.intellij.lang.LanguageFormatting.INSTANCE.addExplicitExtension(org.jetbrains.plugins.scala.ScalaLanguage.INSTANCE,
      new org.jetbrains.plugins.scala.lang.formatting.ScalaFormattingModelBuilder)
    CoreApplicationEnvironment.registerExtensionPoint(area, "com.intellij.lang.importOptimizer", classOf[LanguageExtensionPoint[?]])
    com.intellij.lang.LanguageImportStatements.INSTANCE.addExplicitExtension(org.jetbrains.plugins.scala.ScalaLanguage.INSTANCE,
      new org.jetbrains.plugins.scala.editor.importOptimizer.ScalaImportOptimizer)
    for (name <- Seq("com.intellij.formattingService", "com.intellij.lang.formatter.restriction", "com.intellij.externalFormatProcessor",
                     "com.intellij.preFormatProcessor", "com.intellij.postFormatProcessor", "com.intellij.disabledIndentRangesProvider", "com.intellij.documentWriteAccessGuard")) declare(name)
    area.getExtensionPoint[com.intellij.formatting.service.FormattingService]("com.intellij.formattingService")
      .registerExtension(new com.intellij.formatting.service.CoreFormattingService, root)
    area.getExtensionPoint[com.intellij.lang.LanguageFormattingRestriction]("com.intellij.lang.formatter.restriction")
      .registerExtension(new org.jetbrains.plugins.scala.lang.formatting.ScalaLanguageFormattingRestriction, root)
    area.getExtensionPoint[com.intellij.psi.impl.source.codeStyle.PreFormatProcessor]("com.intellij.preFormatProcessor")
      .registerExtension(new org.jetbrains.plugins.scala.lang.formatting.processors.ScalaDocNewlinedPreFormatProcessor, root)
    val postProcessors = area.getExtensionPoint[com.intellij.psi.impl.source.codeStyle.PostFormatProcessor]("com.intellij.postFormatProcessor")
    postProcessors.registerExtension(new org.jetbrains.plugins.scala.lang.formatting.processors.ScalaBracePostFormatProcessor, root)
    postProcessors.registerExtension(new org.jetbrains.plugins.scala.lang.formatting.processors.ScalaTrailingCommaProcessor, root)
    val symbolIndex = new SymbolIndex
    appServices.register(classOf[com.intellij.psi.stubs.StubIndex])(symbolIndex.stubIndex)
    // The project library table, which the host never changes; LibraryExtensionsManager subscribes to it.
    lazy val libraryTable = tableProxy(classOf[com.intellij.openapi.roots.libraries.LibraryTable], "LibraryTable") {
      case ("addListener", _) => null
    }
    appServices.register(classOf[com.intellij.openapi.roots.libraries.LibraryTablesRegistrar])(new Host_LibraryTablesRegistrar(dispatch {
      case ("LibraryTablesRegistrar.getLibraryTable", Array(_: Project)) => libraryTable
    }))
    // Plugin application services: plain classes.
    appServices.register(classOf[org.jetbrains.plugins.scala.settings.ScalaApplicationSettings])(
      new org.jetbrains.plugins.scala.settings.ScalaApplicationSettings)
    // Which regions the folding builder marks as collapsed by default: the IDE's defaults.
    appServices.register(classOf[org.jetbrains.plugins.scala.settings.ScalaCodeFoldingSettings])(
      new org.jetbrains.plugins.scala.settings.ScalaCodeFoldingSettingsImpl)
    appServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.api.expr.ExpectedTypes])(
      new org.jetbrains.plugins.scala.lang.psi.impl.expr.ExpectedTypesImpl)
    appServices.register(classOf[org.jetbrains.plugins.scala.scalaMeta.ScalaMetaApi])(new scala.meta.intellij.ScalaMetaApiImpl)
    // The editor's read/write-action facade, reached from CodeStyle.getSettings: the host's callers may always read.
    appServices.register(classOf[com.intellij.openapi.editor.elf.Elf])(tableProxy(classOf[com.intellij.openapi.editor.elf.Elf], "Elf") {
      case ("runReadAction", Array(f: kotlin.jvm.functions.Function0[?])) => HostLock.readLocked(f.invoke()).asInstanceOf[AnyRef]
      // Documents are plain platform documents, outside the editor's Elf scope.
      case ("isInElfScope", _) => java.lang.Boolean.FALSE
    })
    // Annotations fetch the global color scheme for presentation (inlay hints, error-stripe-only attributes). The
    // host presents nothing: every key has empty attributes.
    lazy val noColors = tableProxy(classOf[com.intellij.openapi.editor.colors.EditorColorsScheme], "EditorColorsScheme") {
      case ("getAttributes", _) => new com.intellij.openapi.editor.markup.TextAttributes()
    }
    appServices.register(classOf[com.intellij.openapi.editor.colors.EditorColorsManager])(new Host_EditorColorsManager(dispatch {
      case ("EditorColorsManager.getGlobalScheme", _) => noColors
    }))
    // Plugin attribution of logged platform errors (e.g. an annotation range outside its element); the host does not
    // track plugins, so the exception names none.
    appServices.register(classOf[com.intellij.diagnostic.PluginProblemReporter])(
      tableProxy(classOf[com.intellij.diagnostic.PluginProblemReporter], "PluginProblemReporter") {
        case ("createPluginExceptionByClass", Array(message: String, cause, _)) =>
          new com.intellij.diagnostic.PluginException(message, cause.asInstanceOf[Throwable], null)
      })
    // Usage statistics (FUS), which the plugin logs while highlighting: turned off. Building an event would create a
    // device id in the user's Java preferences, which the user's IntelliJ installations share. Every recorder gets
    // no providers, so event logging has nothing to log to and builds no event. The provider map is private platform
    // state; if it changes, this fails at startup.
    locally {
      val holder = new com.intellij.internal.statistic.eventLog.StatisticsEventLogProvidersHolder(
        kotlinx.coroutines.CoroutineScopeKt.CoroutineScope(kotlin.coroutines.EmptyCoroutineContext.INSTANCE))
      type Providers = java.util.Collection[com.intellij.internal.statistic.eventLog.StatisticsEventLoggerProvider]
      val noProviders = new java.util.AbstractMap[String, Providers] {
        override def get(recorder: Any): Providers = java.util.List.of()
        override def entrySet(): java.util.Set[java.util.Map.Entry[String, Providers]] = java.util.Set.of()
      }
      val field = classOf[com.intellij.internal.statistic.eventLog.StatisticsEventLogProvidersHolder].getDeclaredField("eventLoggerProvidersExt")
      field.setAccessible(true)
      field.get(holder).asInstanceOf[java.util.concurrent.atomic.AtomicReference[java.util.Map[String, Providers]]].set(noProviders)
      appServices.register(classOf[com.intellij.internal.statistic.eventLog.StatisticsEventLogProvidersHolder])(holder)
    }
    // Write-safety checks of PSI change notifications: they only check the UI thread, which the host never runs on.
    appServices.register(classOf[com.intellij.openapi.application.TransactionGuard])(new com.intellij.openapi.application.TransactionGuardImpl)
    // Annotations carry their quick fixes, which the platform wraps as intentions when the annotation is created.
    appServices.register(classOf[com.intellij.modcommand.ModCommandService])(new com.intellij.lang.impl.modcommand.ModCommandServiceImpl)
    // Interpolation references give completion its Scala prefix inside plain strings; file paths and URL providers need IDE navigation.
    appServices.register(classOf[com.intellij.psi.PsiReferenceService])(new com.intellij.psi.PsiReferenceServiceImpl)
    appServices.register(classOf[com.intellij.psi.impl.source.resolve.reference.ReferenceProvidersRegistry]) {
      val registry = new com.intellij.psi.impl.source.resolve.reference.ReferenceProvidersRegistryImpl
      val provider = newPrivate(Class.forName("org.jetbrains.plugins.scala.lang.references.InterpolatedStringReferenceProvider")
        .asSubclass(classOf[com.intellij.psi.PsiReferenceProvider]))
      registry.getRegistrar(org.jetbrains.plugins.scala.ScalaLanguage.INSTANCE).registerReferenceProvider(
        com.intellij.patterns.PlatformPatterns.psiElement(classOf[org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral]),
        provider, com.intellij.psi.PsiReferenceRegistrar.DEFAULT_PRIORITY, root)
      registry
    }
    // Java module (JPMS) accessibility: the host has no module-info graph (its modules come from the build), so the
    // platform's graph-less implementation, which treats every class as accessible.
    appServices.register(classOf[com.intellij.psi.JavaModuleGraphHelper])(new com.intellij.psi.DumbJavaModuleGraphHelper)
    // Platform library implementations used by Java PSI of library classes and by PSI copies.
    appServices.register(classOf[com.intellij.psi.util.JavaClassSupers])(new com.intellij.psi.impl.JavaClassSupersImpl)
    appServices.register(classOf[com.intellij.psi.PsiSubstitutorFactory])(new com.intellij.psi.impl.PsiSubstitutorFactoryImpl)
    appServices.register(classOf[com.intellij.psi.impl.file.PsiPackageImplementationHelper])(new com.intellij.core.CorePsiPackageImplementationHelper)
    appServices.register(classOf[com.intellij.psi.impl.source.codeStyle.IndentHelper])(new com.intellij.psi.impl.source.codeStyle.IndentHelperImpl)

    // ---- 2. Language registries and stub schema: explicit extensions only ---------------------
    val parserDefinitions: Seq[ParserDefinition] = Seq(
      new org.jetbrains.plugins.scala.lang.parser.ScalaParserDefinition,
      new org.jetbrains.plugins.scala.lang.parser.Scala3ParserDefinition,
      new org.jetbrains.plugins.scalaDoc.lang.parser.ScalaDocParserDefinition,
      new org.jetbrains.plugins.scalaDirective.lang.parser.ScalaDirectiveParserDefinition,
      // The contents of ScalaDoc links ([[scala.Option]]), parsed lazily inside doc comments.
      new org.jetbrains.plugins.scala.lang.scaladoc.reflinks.ScalaDocRefLinkParserDefinition,
    )
    parserDefinitions.foreach(pd => LanguageParserDefinitions.INSTANCE.addExplicitExtension(pd.getFileNodeType.getLanguage, pd))
    // beforeCompletion and lookup escaping must use Scala's operators and keywords, rather than the Java fallback.
    com.intellij.lang.LanguageNamesValidation.INSTANCE.addExplicitExtension(org.jetbrains.plugins.scala.ScalaLanguage.INSTANCE,
      new org.jetbrains.plugins.scala.lang.refactoring.ScalaNamesValidator)
    // Java sources of mixed projects.
    LanguageParserDefinitions.INSTANCE.addExplicitExtension(com.intellij.lang.java.JavaLanguage.INSTANCE, new com.intellij.lang.java.JavaParserDefinition)
    // Java's typed leaves (PsiIdentifier, PsiKeyword, ...): Java PSI casts its tokens to them.
    LanguageASTFactory.INSTANCE.addExplicitExtension(com.intellij.lang.java.JavaLanguage.INSTANCE, new com.intellij.psi.impl.source.tree.JavaASTFactory)
    com.intellij.platform.syntax.psi.LanguageSyntaxDefinitions.getINSTANCE.addExplicitExtension(
      com.intellij.lang.java.JavaLanguage.INSTANCE, new com.intellij.java.frontback.psi.impl.syntax.JavaSyntaxDefinitionExtension)
    com.intellij.platform.syntax.psi.ElementTypeConverters.getInstance.addExplicitExtension(
      com.intellij.lang.java.JavaLanguage.INSTANCE, new com.intellij.lang.java.syntax.JavaElementTypeConverterExtension)
    // Tokens every language shares (whitespace, bad characters): the converters of a language are all those
    // registered for it, so the common one is registered for Java too.
    com.intellij.platform.syntax.psi.ElementTypeConverters.getInstance.addExplicitExtension(
      com.intellij.lang.java.JavaLanguage.INSTANCE, new com.intellij.platform.syntax.psi.CommonElementTypeConverterFactory)
    val scalaLang = org.jetbrains.plugins.scala.ScalaLanguage.INSTANCE
    val scala3Lang = org.jetbrains.plugins.scala.Scala3Language.INSTANCE
    LanguageASTFactory.INSTANCE.addExplicitExtension(scalaLang, new org.jetbrains.plugins.scala.lang.parser.ScalaASTFactory)
    // A .scala file of a Scala 3 module is Scala 3 (the plugin's substitutor reads the file's module).
    com.intellij.psi.LanguageSubstitutors.getInstance.addExplicitExtension(scalaLang, new org.jetbrains.plugins.scala.lang.parser.ScalaLanguageSubstitutor)
    val viewProviderFactory = new org.jetbrains.plugins.scala.lang.psi.ScFileViewProviderFactory
    LanguageFileViewProviders.INSTANCE.addExplicitExtension(scalaLang, viewProviderFactory)
    LanguageFileViewProviders.INSTANCE.addExplicitExtension(scala3Lang, viewProviderFactory)

    // Stub schema (model): the plugin's serializer/factory registrations and language stub definitions.
    val scalaPlugin = new DefaultPluginDescriptor(PluginId.getId("org.intellij.scala"))
    val declaredStubRegistry = declare("com.intellij.stubElementRegistryExtension").asInstanceOf[ExtensionPointImpl[StubRegistryExtension]]
    declaredStubRegistry
      .registerExtension(new org.jetbrains.plugins.scala.lang.psi.stubs.ScalaStubRegistryExtension, root)
    declaredStubRegistry.registerExtension(new com.intellij.psi.impl.java.stubs.JavaStubRegistryExtension, root)
    // Element type <-> PSI class mapping the Java stub registry fills (private constructor, as a container service).
    appServices.register(classOf[com.intellij.psi.impl.java.stubs.JavaStubElementTypePsiElementMappingRegistry])(
      newPrivate(classOf[com.intellij.psi.impl.java.stubs.JavaStubElementTypePsiElementMappingRegistry]))
    val stubDefinitions = declare("com.intellij.languageStubDefinition")
      .asInstanceOf[ExtensionPointImpl[LanguageExtensionPoint[LanguageStubDefinition]]]
    def stubDefinition(language: String, definition: LanguageStubDefinition) = {
      val bean = new LanguageExtensionPoint[LanguageStubDefinition](language, definition)
      bean.setPluginDescriptor(scalaPlugin)
      bean
    }
    stubDefinitions.registerExtension(stubDefinition("Scala", new org.jetbrains.plugins.scala.lang.psi.stubs.ScalaLanguageStubDefinition), scalaPlugin, root)
    stubDefinitions.registerExtension(stubDefinition("Scala 3", new org.jetbrains.plugins.scala.lang.psi.stubs.Scala3LanguageStubDefinition), scalaPlugin, root)
    stubDefinitions.registerExtension(stubDefinition(com.intellij.lang.java.JavaLanguage.INSTANCE.getID, new com.intellij.psi.impl.java.stubs.JavaStubDefinition), scalaPlugin, root)
    appServices.register(classOf[StubElementRegistryService])(
      new com.intellij.psi.stubs.StubElementRegistryServiceImpl(kotlinx.coroutines.CoroutineScopeKt.CoroutineScope(kotlin.coroutines.EmptyCoroutineContext.INSTANCE)))

    Registry.Companion.setContributedKeys(Descriptors.scalaRegistryKeys(getClass.getClassLoader).asJava)
    Registry.Companion.markAsLoaded()

    val fileTypes = new CoreFileTypeRegistry
    fileTypes.registerFileType(org.jetbrains.plugins.scala.ScalaFileType.INSTANCE, "scala")
    fileTypes.registerFileType(com.intellij.ide.highlighter.JavaFileType.INSTANCE, "java")
    fileTypes.registerFileType(com.intellij.ide.highlighter.JavaClassFileType.INSTANCE, "class")
    // Scala 3 library classes: the .tasty file is decompiled into Scala 3 PSI, its .class sibling is hidden.
    fileTypes.registerFileType(org.jetbrains.plugins.scala.tasty.TastyFileType, "tasty")
    // View providers of binary library files, by file type, as the IDE registers them.
    com.intellij.psi.FileTypeFileViewProviders.INSTANCE.addExplicitExtension(
      com.intellij.ide.highlighter.JavaClassFileType.INSTANCE, new com.intellij.psi.ClassFileViewProviderFactory)
    com.intellij.psi.FileTypeFileViewProviders.INSTANCE.addExplicitExtension(
      org.jetbrains.plugins.scala.tasty.TastyFileType, new org.jetbrains.plugins.scala.tasty.TastyFileViewProviderFactory)
    // The registry is also the FileTypeManager: FileTypeManager.getInstance falls back to an empty manager (which
    // answers a mock file type for everything) when the registry is not one. Lookups go to the core registry above.
    val fileTypeManager = new Host_FileTypeManager(dispatch {
      case ("FileTypeManager.getFileTypeByFileName", Array(name: String)) => fileTypes.getFileTypeByFileName(name)
      case ("FileTypeManager.getFileTypeByExtension", Array(extension: String)) => fileTypes.getFileTypeByExtension(extension)
      case ("FileTypeManager.getFileTypeByFile", Array(f: VirtualFile)) => fileTypes.getFileTypeByFile(f)
      case ("FileTypeManager.findFileTypeByName", Array(name: String)) => fileTypes.findFileTypeByName(name)
      case ("FileTypeManager.getRegisteredFileTypes", _) => fileTypes.getRegisteredFileTypes
      // Files come from the build's source roots and library jars; none is ignored.
      case ("FileTypeManager.isFileIgnored", _) => java.lang.Boolean.FALSE
    })
    FileTypeRegistry.setInstanceSupplier(() => fileTypeManager, root)

    // Class files become PSI as in the IDE: the first decompiler that accepts the file wins; the plugin's comes
    // before the platform's Java decompiler.
    val decompilers = declare("com.intellij.psi.classFileDecompiler")
      .asInstanceOf[ExtensionPointImpl[com.intellij.psi.compiled.ClassFileDecompilers.Decompiler]]
    decompilers.registerExtension(new org.jetbrains.plugins.scala.lang.psi.compiled.ScClassFileDecompiler, root)
    decompilers.registerExtension(new com.intellij.psi.impl.compiled.ClsDecompilerImpl, root)
    appServices.register(classOf[com.intellij.psi.compiled.ClassFileDecompilers])(newPrivate(classOf[com.intellij.psi.compiled.ClassFileDecompilers]))
    // Stubs of library class files are built on demand from their bytes (the core loader kotlinc uses), through the
    // platform's class-file stub builder, which defers to the same decompilers.
    com.intellij.psi.stubs.BinaryFileStubBuilders.INSTANCE.addExplicitExtension(
      org.jetbrains.plugins.scala.tasty.TastyFileType, new org.jetbrains.plugins.scala.tasty.TastyFileStubBuilder)
    com.intellij.psi.stubs.BinaryFileStubBuilders.INSTANCE.addExplicitExtension(
      com.intellij.ide.highlighter.JavaClassFileType.INSTANCE, new com.intellij.psi.impl.compiled.ClassFileStubBuilder)
    appServices.register(classOf[com.intellij.psi.stubs.StubTreeLoader])(new com.intellij.psi.stubs.CoreStubTreeLoader)
    // A binary library file's text (its document, which Java class files' PSI mirrors): decompiled as the IDE
    // registers it, Java class files by the platform's decompiler, TASTy files by the plugin's.
    declare("com.intellij.filetype.decompiler")
    appServices.register(classOf[com.intellij.openapi.fileTypes.BinaryFileTypeDecompilers])(
      newPrivate(classOf[com.intellij.openapi.fileTypes.BinaryFileTypeDecompilers]))
    com.intellij.openapi.fileTypes.BinaryFileTypeDecompilers.getInstance.addExplicitExtension(
      com.intellij.ide.highlighter.JavaClassFileType.INSTANCE, new com.intellij.psi.impl.compiled.ClassFileDecompiler)
    com.intellij.openapi.fileTypes.BinaryFileTypeDecompilers.getInstance.addExplicitExtension(
      org.jetbrains.plugins.scala.tasty.TastyFileType, new org.jetbrains.plugins.scala.tasty.TastyDecompiler)
    // File properties the IDE pushes to files (language levels), which stub builders read to version their stubs; the
    // host pushes none.
    appServices.register(classOf[com.intellij.openapi.roots.impl.PushedFilePropertiesRetriever])(
      tableProxy(classOf[com.intellij.openapi.roots.impl.PushedFilePropertiesRetriever], "PushedFilePropertiesRetriever") {
        case ("dumpSortedPushedProperties", _) => java.util.List.of()
      })
    val libraryFiles = new LibraryFileSystem
    // Class root → its attached source root.
    val librarySources = new ConcurrentHashMap[VirtualFile, VirtualFile]()
    // The platform's file-based indexes: only the file-name index exists in the host, answered from library roots.
    appServices.register(classOf[com.intellij.util.indexing.FileBasedIndex])(new Host_FileBasedIndex(dispatch {
      case ("FileBasedIndex.processFilesContainingAnyKey",
            Array(id: com.intellij.util.indexing.ID[?, ?], names: java.util.Collection[?], scope: com.intellij.psi.search.GlobalSearchScope, _, _,
                  processor: com.intellij.util.Processor[?])) if id == com.intellij.psi.search.FilenameIndex.NAME =>
        val p = processor.asInstanceOf[com.intellij.util.Processor[VirtualFile]]
        val matching = names.asScala.toSeq.flatMap(n => libraryFiles.filesNamed(n.toString)).filter(scope.contains)
        java.lang.Boolean.valueOf(matching.forall(p.process))
    }))
    // Persistent file attributes: the plugin caches "is this class file Scala?" in them. The host persists nothing,
    // so every read misses (the plugin recomputes) and writes are discarded.
    appServices.register(classOf[com.intellij.openapi.vfs.newvfs.ManagingFS])(new Host_ManagingFS(dispatch {
      case ("ManagingFS.readAttribute", _) => null
      case ("ManagingFS.writeAttribute", _) =>
        new com.intellij.openapi.vfs.newvfs.AttributeOutputStream(new com.intellij.openapi.util.io.BufferExposingByteArrayOutputStream()) {
          override def writeEnumeratedString(value: String): Unit = writeUTF(value)
        }
    }))

    // ---- 3. Project object -----------------------------------------------------------------
    // The platform's modification tracker: it counts the tree changes edits produce (see HostPsiManager).
    lazy val psiModTracker = new com.intellij.psi.impl.PsiModificationTrackerImpl(project)
    lazy val projectBus = new MessageBusFactoryImpl().createMessageBus(busOwner, appBus)
    lazy val moduleModel = new ModuleModel(project)
    lazy val project: Project = tableProxy(classOf[Project], "Project") {
      case ("getService", Array(c: Class[?])) => projectServices.get(c)
      case ("getServiceIfCreated", Array(c: Class[?])) => projectServices.get(c)
      case ("getExtensionArea", _) => area
      case ("getMessageBus", _) => projectBus
      case ("getName", _) => "scala-psi-host"
      case ("getBasePath", _) => "/scala-psi-host"
      case ("getLocationHash", _) => "scala-psi-host"
      case ("isDisposed", _) => java.lang.Boolean.FALSE
      case ("isOpen" | "isInitialized", _) => java.lang.Boolean.TRUE
      case ("isDefault", _) => java.lang.Boolean.FALSE
      case ("getUserData", Array(k: Key[?])) => projectData.getUserData(k).asInstanceOf[AnyRef]
      case ("putUserData", Array(k: Key[?], v)) =>
        projectData.putUserData(k.asInstanceOf[Key[AnyRef]], v); null
    }
    // The host's file store: view providers of project sources and of library files, keyed by virtual file; sources
    // register themselves, library files are created on first request. Other light files are the platform's
    // temporary copies (a reparse makes one per edit): such a file holds its own provider, as in the platform's
    // LightFileViewProviderCache, so the copy is collected once no one holds it.
    val viewProviders = new ConcurrentHashMap[VirtualFile, FileViewProvider]()
    val temporaryViewProvider = Key.create[FileViewProvider]("scala-psi-host.temporaryViewProvider")
    def isTemporary(f: VirtualFile) = f.isInstanceOf[com.intellij.testFramework.LightVirtualFile] && !f.isInstanceOf[SourceFile]
    def cachedViewProvider(f: VirtualFile): FileViewProvider =
      if (isTemporary(f)) f.getUserData(temporaryViewProvider) else viewProviders.get(f)
    val directories = new ConcurrentHashMap[VirtualFile, com.intellij.psi.PsiDirectory]()
    var psiManagerRef: PsiManager = null
    def viewProvider(f: VirtualFile): FileViewProvider = Option(cachedViewProvider(f)).getOrElse(viewProviders.computeIfAbsent(f, {
      // Physical (event system enabled), as the IDE's library files: a non-physical file loads its syntax tree up
      // front, while a physical one stays stub-based until something navigates into it.
      case lib: LibraryFile if com.intellij.psi.FileTypeFileViewProviders.INSTANCE.forFileType(lib.getFileType) != null =>
        com.intellij.psi.FileTypeFileViewProviders.INSTANCE.forFileType(lib.getFileType).createFileViewProvider(lib, null, psiManagerRef, true)
      // Library sources, as project sources below (the plugin's substitutor reads Scala 3 from the source jar's name).
      case lib: LibraryFile if lib.isLibrarySource => sourceViewProvider(lib)
      // Project sources are physical too: their syntax tree follows document commits (reparse) and can be dropped
      // when no one holds it. The language's view provider when it has one (Scala), the platform's default otherwise.
      case source: SourceFile => sourceViewProvider(source)
      case other => CallLog.unsupported(s"FileManager.findViewProvider for a file the host does not own: $other")
    }))
    // As the IDE's FileManagerImpl: the file type's language, as substituted for this file (Scala 3 modules).
    def sourceViewProvider(source: VirtualFile): FileViewProvider = {
      val language = com.intellij.psi.LanguageSubstitutors.getInstance.substituteLanguage(
        source.getFileType.asInstanceOf[com.intellij.openapi.fileTypes.LanguageFileType].getLanguage, source, project)
      Option(LanguageFileViewProviders.INSTANCE.forLanguage(language))
        .map(_.createFileViewProvider(source, language, psiManagerRef, true))
        .getOrElse(new com.intellij.psi.SingleRootFileViewProvider(psiManagerRef, source, true))
    }
    val fileManager = tableProxy(classOf[com.intellij.psi.impl.file.impl.FileManagerEx], "FileManager") {
      case ("findCachedViewProvider", Array(f: VirtualFile)) => cachedViewProvider(f)
      // One view provider per file (no code-insight contexts).
      case ("findCachedViewProviders", Array(f: VirtualFile)) => Option(cachedViewProvider(f)).toList.asJava
      case ("getCachedPsiFile", Array(f: VirtualFile, _*)) =>
        Option(cachedViewProvider(f)).map(vp => vp.getPsi(vp.getBaseLanguage)).orNull
      case ("setViewProvider", Array(f: VirtualFile, vp)) =>
        if (isTemporary(f)) f.putUserData(temporaryViewProvider, vp.asInstanceOf[FileViewProvider])
        else if (vp == null) viewProviders.remove(f)
        else viewProviders.put(f, vp.asInstanceOf[FileViewProvider])
        null
      case ("forceReload", Array(f: VirtualFile)) if isTemporary(f) &&
          Option(cachedViewProvider(f)).forall(vp => !vp.supportsSendingPsiEvents()) =>
        // FileManagerImpl drops nonphysical providers on reload; it never reloads their backing file from disk.
        f.putUserData(temporaryViewProvider, null); null
      case ("findViewProvider", Array(f: VirtualFile)) => viewProvider(f)
      case ("findFile", Array(f: VirtualFile)) =>
        if (f.isDirectory) null else { val vp = viewProvider(f); vp.getPsi(vp.getBaseLanguage) }
      case ("findDirectory", Array(f: VirtualFile)) =>
        if (!f.isDirectory) null
        else directories.computeIfAbsent(f, d => new com.intellij.psi.impl.file.PsiDirectoryImpl(psiManagerRef.asInstanceOf[com.intellij.psi.impl.PsiManagerEx], d))
    }
    // Library files (classes and attached sources) live in the host's library file system; every other file is a
    // project source the host was given.
    def isLibrary(f: VirtualFile) = f.getFileSystem.isInstanceOf[LibraryFileSystem]
    def isLibrarySource(f: VirtualFile) = f match {
      case lib: LibraryFile => lib.isLibrarySource
      case _ => false
    }
    def isLibraryClass(f: VirtualFile) = isLibrary(f) && !isLibrarySource(f)
    val fileIndex = tableProxy(classOf[com.intellij.openapi.roots.ProjectFileIndex], "ProjectFileIndex") {
      case ("isInLibrary", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(isLibrary(f))
      case ("isInLibraryClasses", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(isLibraryClass(f))
      case ("isInLibrarySource", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(isLibrarySource(f))
      // The host has no workspace model: no library or SDK entity contains a file.
      case ("findContainingLibraries" | "findContainingSdks", _) => java.util.List.of()
      // Files opened in a module; other files (light files, libraries) have none.
      case ("getModuleForFile", Array(f: VirtualFile, _*)) => moduleModel.ofFile(f).map(_.module).orNull
      case ("isInSourceContent" | "isInContent", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(!isLibrary(f))
      // Sources: the project's and the libraries' attached ones.
      case ("isInSource", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(!isLibraryClass(f))
      case ("isInTestSourceContent", _) => java.lang.Boolean.FALSE
      // Every source file the host was given is a production source; the plugin's filter scopes ask by root type.
      case ("isUnderSourceRootOfType", Array(f: VirtualFile, types: java.util.Set[?])) =>
        java.lang.Boolean.valueOf(!isLibrary(f) && types.contains(org.jetbrains.jps.model.java.JavaSourceRootType.SOURCE))
    }
    projectServices.register(classOf[com.intellij.openapi.roots.ProjectFileIndex])(fileIndex)
    // Java stub eligibility reads a persisted level as the source-content marker. The host owns source membership,
    // and uses the same highest Java level here as in JavaPsiImplementationHelper, without persisting VFS attributes.
    appServices.register(classOf[com.intellij.pom.java.InternalPersistentJavaLanguageLevelReaderService])(
      tableProxy(classOf[com.intellij.pom.java.InternalPersistentJavaLanguageLevelReaderService], "JavaLanguageLevelReader") {
        case ("getPersistedLanguageLevel", Array(f: VirtualFile)) =>
          if (moduleModel.ofFile(f).isDefined) com.intellij.pom.java.LanguageLevel.HIGHEST else null
      })
    // The same answers through the platform's facade (ClassFileViewProvider asks it before building Java PSI).
    projectServices.register(classOf[com.intellij.openapi.roots.FileIndexFacade])(new Host_FileIndexFacade(project, dispatch {
      case ("FileIndexFacade.isInLibraryClasses", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(isLibraryClass(f))
      case ("FileIndexFacade.isInLibrary", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(isLibrary(f))
      case ("FileIndexFacade.isInLibrarySource", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(isLibrarySource(f))
      // Roots change only when a library or the JDK is mounted, which also counts as a PSI change.
      case ("FileIndexFacade.getRootModificationTracker", _) => psiModTracker
      case ("FileIndexFacade.getModuleForFile", Array(f: VirtualFile)) => moduleModel.ofFile(f).map(_.module).orNull
      case ("FileIndexFacade.isInSourceContent" | "FileIndexFacade.isInContent", Array(f: VirtualFile)) =>
        java.lang.Boolean.valueOf(!isLibrary(f))
      case ("FileIndexFacade.isInSource", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(!isLibraryClass(f))
    }))
    // Tree changes reach the plugin's listener, which decides which of its caches an edit invalidates.
    val psiManager = new HostPsiManager(project, psiModTracker, fileManager, psiModTracker,
      java.util.List.of(new org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiChangeListener(project)))
    psiManagerRef = psiManager
    projectServices.register(classOf[PsiManager])(psiManager)
    projectServices.register(classOf[PsiModificationTracker])(psiModTracker)
    // PSI <-> document synchronization without the IDE's global editor event multicaster.
    projectServices.register(classOf[com.intellij.psi.PsiDocumentManager]) {
      val documents = new HostPsiDocumentManager(project)
      // Commits are explicit and synchronous (ScalaPsiHost.edit), never the IDE's background commit after typing.
      documents.disableBackgroundCommit(root)
      documents
    }
    // Platform library implementations, constructed by the host (no container wiring).
    projectServices.register(classOf[com.intellij.psi.util.CachedValuesManager])(
      new com.intellij.util.CachedValuesManagerImpl(project, new com.intellij.psi.impl.PsiCachedValuesFactory(project)))
    projectServices.register(classOf[com.intellij.psi.PsiFileFactory])(new com.intellij.psi.impl.PsiFileFactoryImpl(psiManager))
    projectServices.register(classOf[com.intellij.openapi.roots.ProjectRootManager])(new Host_ProjectRootManager(dispatch {
      case ("ProjectRootManager.getFileIndex", _) => fileIndex
      // Each host module carries its root manager (see ScalaPsiHost.newModule).
      case ("ProjectRootManager.getModuleRootManager", Array(m: Module)) =>
        m.getService(classOf[com.intellij.openapi.roots.ModuleRootManager])
    }))
    // The host's indexes are always ready before it answers queries.
    projectServices.register(classOf[DumbService])(new Host_DumbService(dispatch {
      case ("DumbService.isDumb", _) => java.lang.Boolean.FALSE
      // Index-free "alternative" resolution exists only for dumb mode, which the host never enters.
      case ("DumbService.isAlternativeResolveEnabled", _) => java.lang.Boolean.FALSE
      // No extension is dumb-aware-filtered away: the host is never dumb.
      case ("DumbService.filterByDumbAwareness", Array(c: java.util.Collection[?])) => java.util.List.copyOf(c)
      case ("DumbService.getProject", _) => project
      // Dumb-mode tasks (synthetic class registration) run to completion on the caller's thread.
      case ("DumbService.queueTask", Array(task: DumbModeTask)) =>
        try task.performInDumbMode(new EmptyProgressIndicator(ModalityState.nonModal()))
        finally Disposer.dispose(task)
        null
    }))
    // The same holds for dependencies: libraries are indexed before the host answers, so highlighting is type-aware.
    projectServices.register(classOf[com.intellij.openapi.project.IncompleteDependenciesService])(
      tableProxy(classOf[com.intellij.openapi.project.IncompleteDependenciesService], "IncompleteDependenciesService") {
        case ("getState" | "getStateUnsafe", _) => com.intellij.openapi.project.IncompleteDependenciesService.DependenciesState.COMPLETE
      })
    // Every file gets full highlighting ("All Problems"); the IDE's per-file level is a UI setting.
    projectServices.register(classOf[com.intellij.codeInsight.daemon.impl.analysis.HighlightingLevelManager])(new Host_HighlightingLevelManager(dispatch {
      case ("HighlightingLevelManager.shouldHighlight" | "HighlightingLevelManager.shouldInspect", _) => java.lang.Boolean.TRUE
      case ("HighlightingLevelManager.runEssentialHighlightingOnly", _) => java.lang.Boolean.FALSE
    }))
    projectServices.register(classOf[com.intellij.openapi.module.ModuleManager])(new Host_ModuleManager(dispatch {
      case ("ModuleManager.getModules", _) => moduleModel.all.map(_.module).toArray
      case ("ModuleManager.findModuleByName", Array(name: String)) => moduleModel.named(name).map(_.module).orNull
    }))
    // Compiler settings per module, configured from the build's scalac options as the plugin's importers do.
    projectServices.register(classOf[org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration])(
      new org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration(project))
    // PSI write path: the plugin edits non-physical copies (e.g. for-comprehension desugaring).
    projectServices.register(classOf[com.intellij.pom.PomModel])(
      classOf[com.intellij.pom.PomModel].cast(newPrivate(
        Class.forName("com.intellij.psi.impl.source.PostprocessReformattingAspectImpl$LangPomModel"), project)))
    projectServices.register(classOf[com.intellij.pom.tree.TreeAspect])(new com.intellij.pom.tree.TreeAspect)
    projectServices.register(classOf[com.intellij.psi.SmartPointerManager])(new com.intellij.psi.impl.smartPointers.SmartPointerManagerImpl(project))
    projectServices.register(classOf[com.intellij.psi.PsiElementFactory])(new com.intellij.psi.impl.PsiElementFactoryImpl(project))
    projectServices.register(classOf[com.intellij.psi.PsiNameHelper])(com.intellij.psi.impl.PsiNameHelperImpl.getInstance())
    // One scope for everything, as one scope per module: the plugin keys its caches by scope, so a new object per
    // request would add entries on every lookup that are never found again.
    val everythingScope = new com.intellij.psi.search.EverythingGlobalScope(project)
    projectServices.register(classOf[com.intellij.psi.search.ProjectScopeBuilder])(new Host_ProjectScopeBuilder(dispatch {
      case (m, _) if m.startsWith("ProjectScopeBuilder.build") => everythingScope
    }))
    // Class and package lookup: the platform's JavaPsiFacadeImpl over element finders, as in the IDE. The plugin's
    // finders come with the host's LibraryClassFinder, which takes the place of the IDE's PsiElementFinderImpl.
    projectServices.register(classOf[com.intellij.psi.JvmPsiConversionHelper])(new com.intellij.psi.impl.JvmPsiConversionHelperImpl)
    projectServices.register(classOf[com.intellij.psi.impl.source.resolve.ResolveCache])(new com.intellij.psi.impl.source.resolve.ResolveCache(project))
    projectServices.register(classOf[com.intellij.psi.impl.migration.PsiMigrationManager])(new com.intellij.psi.impl.migration.PsiMigrationManager(project))
    // External (XML) annotations of library classes: the host attaches none.
    projectServices.register(classOf[com.intellij.codeInsight.ExternalAnnotationsManager])(new Host_ExternalAnnotationsManager(dispatch {
      case ("ExternalAnnotationsManager.findExternalAnnotationsFiles", _) => null
      case ("ExternalAnnotationsManager.findExternalAnnotations", Array(_)) => com.intellij.psi.PsiAnnotation.EMPTY_ARRAY
      case ("ExternalAnnotationsManager.findExternalAnnotations", Array(_, _)) => java.util.List.of()
    }))
    projectServices.register(classOf[com.intellij.codeInsight.InferredAnnotationsManager])(new HostInferredAnnotationsManager)
    // Nullability annotations of library Java types: the host configures none (the IDE's defaults come from settings).
    projectServices.register(classOf[com.intellij.codeInsight.NullableNotNullManager])(new Host_NullableNotNullManager(project, dispatch {
      case ("NullableNotNullManager.getNullityDefault", _) => com.intellij.codeInsight.ContextNullabilityInfo.EMPTY
      case ("NullableNotNullManager.findEffectiveNullabilityInfo", _) => null
    }))
    // Java class files navigate to their sources through this helper, as the IDE's: the source file of the same
    // package and name in the sources attached to the class's library, or the class file itself (a library without
    // sources).
    projectServices.register(classOf[com.intellij.psi.impl.JavaPsiImplementationHelper])(new Host_JavaPsiImplementationHelper(dispatch {
      case ("JavaPsiImplementationHelper.getClsFileNavigationElement", Array(f: com.intellij.psi.PsiJavaFile)) =>
        val source = for {
          classFile <- Option(f.getVirtualFile).collect { case lib: LibraryFile => lib }
          sourceRoot <- Option(librarySources.get(classFile.root))
          sourceName <- f.getClasses.headOption.collect { case c: com.intellij.psi.impl.compiled.ClsClassImpl => c.getSourceFileName }
          directory = classFile.entry.lastIndexOf('/') match { case -1 => "" case i => classFile.entry.substring(0, i + 1) }
          sourceFile <- Option(sourceRoot.findFileByRelativePath(directory + sourceName))
          psi <- Option(psiManagerRef.findFile(sourceFile))
        } yield psi
        source.getOrElse(f)
      // Java sources parse at the highest language level: it accepts the syntax of every Java release the build may
      // target (the build's own release is not read yet).
      case ("JavaPsiImplementationHelper.getEffectiveLanguageLevel", _) => com.intellij.pom.java.LanguageLevel.HIGHEST
    }))
    // Java class-file parameters compiled without names: the IDE suggests names from Java code style (which needs
    // Java code-style settings and IDE registry keys); without a suggestion the platform names them `p`, `p1`, ...
    projectServices.register(classOf[com.intellij.psi.codeStyle.JavaCodeStyleManager])(new Host_JavaCodeStyleManager(dispatch {
      // suggestCompiledParameterName takes the first suggestion: none.
      case ("JavaCodeStyleManager.suggestVariableName", _) => com.intellij.psi.codeStyle.SuggestedNameInfo.NULL_INFO
    }))
    // Scala enumeration uses the existing SymbolIndex through the plugin's cache. Java binaries remain unavailable.
    lazy val scalaShortNames = new org.jetbrains.plugins.scala.caches.ScalaShortNamesCache(using project)
    projectServices.register(classOf[com.intellij.psi.search.PsiShortNamesCache])(new Host_PsiShortNamesCache(dispatch {
      case ("PsiShortNamesCache.getClassesByName", Array(name: String, scope: com.intellij.psi.search.GlobalSearchScope)) =>
        scalaShortNames.getClassesByName(name, scope)
      case ("PsiShortNamesCache.getAllClassNames", _) => scalaShortNames.getAllClassNames.sorted
    }))
    // Annotation lookups cache by owner; the host has no mirrored elements (light copies of sources), so an owner is
    // its own cache key.
    projectServices.register(classOf[com.intellij.codeInsight.AnnotationCacheOwnerNormalizer])(new Host_AnnotationCacheOwnerNormalizer(dispatch {
      case ("AnnotationCacheOwnerNormalizer.doNormalize", Array(owner)) => owner
    }))
    // Auto-import exclusions, read by quick fixes that look for importable implicit instances: the defaults.
    projectServices.register(classOf[com.intellij.codeInsight.JavaProjectCodeInsightSettings])(new com.intellij.codeInsight.JavaProjectCodeInsightSettings)
    // Code-insight contexts (multiverse: one file seen from several modules): off, so every file is in the
    // default context, as in the IDE without multiverse.
    projectServices.register(classOf[com.intellij.codeInsight.multiverse.CodeInsightContextManager])(
      tableProxy(classOf[com.intellij.codeInsight.multiverse.CodeInsightContextManager], "CodeInsightContextManager") {
        case ("getCodeInsightContext" | "getCodeInsightContextRaw" | "getPreferredContext", _) =>
          com.intellij.codeInsight.multiverse.CodeInsightContexts.defaultContext()
        case ("getCodeInsightContexts", _) => java.util.List.of(com.intellij.codeInsight.multiverse.CodeInsightContexts.defaultContext())
      })
    // Java code style of decompiled class files (indents): the platform's core defaults, as headless compilers use.
    appServices.register(classOf[com.intellij.psi.codeStyle.JavaFileCodeStyleFacadeFactory])(
      tableProxy(classOf[com.intellij.psi.codeStyle.JavaFileCodeStyleFacadeFactory], "JavaFileCodeStyleFacadeFactory") {
        case ("createFacade", _) => new com.intellij.core.CoreJavaFileCodeStyleFacade
      })
    // The IDE decompiles a Java class file (its text, for the class's PSI mirror) in its default project, as a
    // throwaway class-file PSI; the host's one project takes that role.
    appServices.register(classOf[com.intellij.openapi.project.DefaultProjectFactory])(new Host_DefaultProjectFactory(dispatch {
      case ("DefaultProjectFactory.getDefaultProject", _) => project
    }))
    projectServices.register(classOf[com.intellij.psi.codeStyle.CodeStyleManager])(
      new com.intellij.psi.impl.source.codeStyle.CodeStyleManagerImpl(project))
    // No language injection in the host: every file is its own top-level file.
    projectServices.register(classOf[com.intellij.lang.injection.InjectedLanguageManager])(new Host_InjectedLanguageManager(dispatch {
      case ("InjectedLanguageManager.getTopLevelFile", Array(e: com.intellij.psi.PsiElement)) => e.getContainingFile
      case ("InjectedLanguageManager.findInjectedElementAt", _) => null
      case ("InjectedLanguageManager.isInjectedViewProvider", _) => java.lang.Boolean.FALSE
      case ("InjectedLanguageManager.getCachedInjectedDocumentsInRange", _) => java.util.List.of()
      // There are no registered injectors, so enumeration has no injected fragments to visit.
      case ("InjectedLanguageManager.enumerate", _) => null
    }))
    // JVM-language-neutral class lookup that JavaPsiFacadeImpl also consults; no other JVM language provides classes.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.lang.jvm.facade.JvmElementProvider.EP_NAME.getName,
      classOf[com.intellij.lang.jvm.facade.JvmElementProvider])
    projectServices.register(classOf[com.intellij.lang.jvm.facade.JvmFacade])(new com.intellij.lang.jvm.facade.JvmFacadeImpl(project))
    projectServices.register(classOf[com.intellij.psi.impl.file.impl.JavaFileManager])(
      tableProxy(classOf[com.intellij.psi.impl.file.impl.JavaFileManager], "JavaFileManager")(PartialFunction.empty))
    projectServices.register(classOf[com.intellij.psi.JavaPsiFacade])(new com.intellij.psi.impl.JavaPsiFacadeImpl(project, null))
    // Resolution inside Java sources (mixed projects).
    projectServices.register(classOf[com.intellij.psi.PsiResolveHelper])(new com.intellij.psi.impl.source.resolve.PsiResolveHelperImpl(project))
    // ScalaDocParserDefinition reads TAB_SIZE from code style: one fixed settings object, never the scheme manager.
    projectServices.register(classOf[com.intellij.psi.codeStyle.ProjectCodeStyleSettingsManager]) {
      val m = new com.intellij.psi.codeStyle.ProjectCodeStyleSettingsManager(project, /* loadExtensions = */ false)
      m.USE_PER_PROJECT_SETTINGS = true
      m.setMainProjectCodeStyle(new com.intellij.psi.codeStyle.CodeStyleSettings)
      m
    }
    projectServices.register(classOf[com.intellij.psi.impl.ResolveScopeManager])(new Host_ResolveScopeManager(dispatch {
      // A module's files resolve in the module's scope; files outside modules (light files, library classes) see
      // everything.
      case ("ResolveScopeManager.getResolveScope" | "ResolveScopeManager.getUseScope", Array(e: com.intellij.psi.PsiElement)) =>
        val file = Option(e.getContainingFile).map(_.getOriginalFile.getViewProvider.getVirtualFile)
        file.flatMap(moduleModel.ofFile).map(moduleModel.resolveScope).getOrElse(everythingScope)
      case ("ResolveScopeManager.getDefaultResolveScope", Array(f: VirtualFile)) =>
        moduleModel.ofFile(f).map(moduleModel.resolveScope).getOrElse(everythingScope)
    }))
    // Index-backed Scala symbol lookup: the real class, answered by the host's StubIndex.
    projectServices.register(classOf[org.jetbrains.plugins.scala.caches.ScalaShortNamesCacheManager])(
      new org.jetbrains.plugins.scala.caches.ScalaShortNamesCacheManager(using project))
    // LibraryExtensionsManager is built by its own constructor. With library extensions disabled it loads nothing,
    // so dynamic extension lookups (SyntheticMembersInjector, ScalaMacroEvaluator) answer "none". It keeps a
    // PropertiesComponent that it reads only when extensions are enabled.
    projectServices.register(classOf[com.intellij.ide.util.PropertiesComponent])(new Host_PropertiesComponent(dispatch(PartialFunction.empty)))
    projectServices.register(classOf[org.jetbrains.plugins.scala.components.libextensions.LibraryExtensionsManager])(
      new org.jetbrains.plugins.scala.components.libextensions.LibraryExtensionsManager(project))
    // Settings are plain state beans with defaults (later: from LSP configuration).
    projectServices.register(classOf[org.jetbrains.plugins.scala.settings.ScalaProjectSettings]) {
      val settings = new org.jetbrains.plugins.scala.settings.ScalaProjectSettings(project)
      // Library extensions are jars downloaded from build resolvers and loaded into the IDE: out of scope.
      settings.setEnableLibraryExtensions(false)
      // Where errors come from: the build's compiler (the caller adds its problems) or the plugin's type-aware checks.
      settings.setCompilerHighlightingScala3(config.compilerErrorsScala3)
      settings.setCompilerHighlightingScala2(config.compilerErrorsScala2)
      // Type mismatches as diagnostics on the expression, not as editor inlay hints (which LSP clients lack).
      settings.setTypeMismatchHints(false)
      settings
    }
    projectServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager])(
      new org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager(using project))
    projectServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes])(
      new org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes(using new ProjectContext(project)))
    projectServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.SyntheticClasses])(
      new org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.SyntheticClasses(project))
    // Scala 3 type operations (union joins).
    projectServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.types.Scala3Bounds])(
      org.jetbrains.plugins.scala.lang.psi.types.Scala3Bounds(project))
    projectServices.register(classOf[org.jetbrains.plugins.scala.lang.macros.evaluator.ScalaMacroEvaluator])(
      new org.jetbrains.plugins.scala.lang.macros.evaluator.ScalaMacroEvaluator(project))
    // Private light services (@Service): the container builds them by class name, and so does the host.
    Seq(
      "org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType$SubstitutorCacheService",
      "org.jetbrains.plugins.scala.util.UnloadAwareDisposable$UnloadAwareDisposableService",
      "org.jetbrains.plugins.scala.lang.psi.types.ScMatchType$MatchTypeReductionCacheService",
    ).foreach { name =>
      val cls = Class.forName(name).asInstanceOf[Class[AnyRef]]
      projectServices.register(cls)(newPrivate(cls))
    }

    // Element finders are built last: their constructors fetch project services.
    CoreApplicationEnvironment.registerExtensionPoint(area, "com.intellij.java.elementFinder", classOf[com.intellij.psi.PsiElementFinder])
    val finders = area.getExtensionPoint[com.intellij.psi.PsiElementFinder]("com.intellij.java.elementFinder")
      .asInstanceOf[ExtensionPointImpl[com.intellij.psi.PsiElementFinder]]
    finders.registerExtension(new LibraryClassFinder(project, libraryFiles), root)
    finders.registerExtension(new org.jetbrains.plugins.scala.finder.ScalaClassFinder(project), root)
    finders.registerExtension(new org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.SyntheticClassElementFinder(project), root)

    // ---- 4. Project startup the PSI depends on -------------------------------------------------
    // The plugin's own activity registers the synthetic classes (Any, Int, ...) through DumbService.queueTask.
    new org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.RegisterSyntheticClassesProjectActivity().execute(project)

    // A source file was added or removed, or a library mounted: what the IDE's PsiModificationTrackerImpl and the
    // plugin's ScalaPsiChangeListener do for a change under a directory (not a Scala element).
    def psiChanged(): Unit = HostLock.write {
      psiModTracker.incCounter()
      org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager.instance(project).clearOnNonScalaChange()
    }

    new ScalaPsiHost(config.stateDir, project, symbolIndex, libraryFiles, librarySources, moduleModel, () => psiChanged())
  }

  private def newPrivate[T](cls: Class[T], args: AnyRef*): T = {
    val ctor = cls.getDeclaredConstructors.find(_.getParameterCount == args.size)
      .getOrElse(throw new IllegalStateException(s"no ${args.size}-argument constructor in ${cls.getName}"))
    ctor.setAccessible(true)
    cls.cast(ctor.newInstance(args*))
  }
}
