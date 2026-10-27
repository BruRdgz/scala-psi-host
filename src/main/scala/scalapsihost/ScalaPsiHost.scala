package scalapsihost

import com.intellij.core.{CoreApplicationEnvironment, CoreFileTypeRegistry}
import com.intellij.psi.codeStyle.FileIndentOptionsProvider
import com.intellij.psi.impl.source.tree.TreeCopyHandler
import com.intellij.lang.{DefaultASTFactory, DefaultASTFactoryImpl, LanguageASTFactory, LanguageExtensionPoint, LanguageParserDefinitions, ParserDefinition, PsiBuilderFactory}
import com.intellij.lang.impl.PsiBuilderFactoryImpl
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.{Application, ApplicationManager, ModalityState}
import com.intellij.openapi.extensions.{DefaultPluginDescriptor, Extensions, PluginId}
import com.intellij.openapi.extensions.impl.{ExtensionPointImpl, ExtensionsAreaImpl}
import com.intellij.openapi.fileTypes.FileTypeRegistry
import com.intellij.openapi.progress.{EmptyProgressIndicator, ProgressManager}
import com.intellij.openapi.progress.impl.CoreProgressManager
import com.intellij.openapi.project.{DumbModeTask, DumbService, Project}
import com.intellij.openapi.util.{Disposer, Key, SimpleModificationTracker, UserDataHolderBase}
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
 * @param stateDir        the only place the platform writes to disk (index ID enumeration under `idea.system.path`)
 * @param scalaPluginJars the Scala plugin's jars; they must also be on the classpath
 */
final case class HostConfig(stateDir: Path, scalaPluginJars: Seq[Path])

/**
 * Scala PSI and type checker hosted without an IntelliJ application container. The host installs three fixed
 * tables — an `Application` object, a root extension area and a `Project` object — and then only consumes the
 * plugin's PSI. Whatever is not in a table fails loudly and is recorded in [[CallLog]].
 */
final class ScalaPsiHost private (val project: Project, libraryIndex: LibraryIndex) {

  def parse(text: String, features: ScalaFeatures = ScalaFeatures.default): ScalaFile =
    ScalaPsiElementFactory.createScalaFileFromText(text, features)(using new ProjectContext(project))

  def indexLibrary(jar: Path, includeEntry: String => Boolean): LibraryIndexReport =
    libraryIndex.indexJar(project, jar, includeEntry)
}

object ScalaPsiHost {
  @volatile private var started: Option[ScalaPsiHost] = None

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
    lazy val app: Application = tableProxy(classOf[Application], "Application") {
      case ("getService", Array(c: Class[?])) => appServices.get(c)
      case ("getServiceIfCreated", Array(c: Class[?])) => appServices.get(c)
      case ("getExtensionArea", _) => area
      case ("getMessageBus", _) => appBus
      // The host has no UI thread and no write actions: every caller may read.
      case ("isDispatchThread", _) => java.lang.Boolean.FALSE
      case ("isWriteIntentLockAcquired", _) => java.lang.Boolean.FALSE
      case ("isReadAccessAllowed", _) => java.lang.Boolean.TRUE
      case ("isWriteAccessAllowed", _) => java.lang.Boolean.FALSE
      case ("assertReadAccessAllowed" | "assertIsNonDispatchThread" | "assertReadAccessNotAllowed", _) => null
      case ("isUnitTestMode" | "isInternal" | "isEAP" | "isDisposed", _) => java.lang.Boolean.FALSE
      case ("isHeadlessEnvironment" | "isCommandLine", _) => java.lang.Boolean.TRUE
      case ("getUserData", Array(k: Key[?])) => appData.getUserData(k).asInstanceOf[AnyRef]
      // Extension points resolve their declared class through the component manager.
      case ("loadClass", Array(name: String, _)) => Class.forName(name)
      case ("runReadAction", Array(r: Runnable)) => r.run(); null
      case ("runReadAction", Array(c: com.intellij.openapi.util.Computable[?])) => c.compute().asInstanceOf[AnyRef]
    }
    ApplicationManager.setApplication(app, root)
    Extensions.setRootArea(area)

    val classpathJars = System.getProperty("java.class.path").split(File.pathSeparator).toSeq
      .filter(_.endsWith(".jar")).map(Path.of(_))
    val epDecls = Descriptors.extensionPoints(classpathJars, config.scalaPluginJars.toSet)
    def declare(name: String): ExtensionPointImpl[?] = {
      val d = epDecls.getOrElse(name, throw new IllegalStateException(s"extension point is not declared on the classpath: $name"))
      area.registerExtensionPoint(d.name, d.className, d.kind, d.dynamic)
      area.getExtensionPoint(name).asInstanceOf[ExtensionPointImpl[?]]
    }
    // The plugin's own extension points, with no extensions.
    epDecls.values.filter(_.fromScalaPlugin).foreach(d => declare(d.name))
    // Platform extension points the plugin's PSI queries, with no extensions. Their declarations live in IDE
    // descriptors that are not on this classpath; the platform's own core environment declares them in code too.
    CoreApplicationEnvironment.registerExtensionPoint(area, FileIndentOptionsProvider.EP_NAME, classOf[FileIndentOptionsProvider])
    CoreApplicationEnvironment.registerExtensionPoint(area, TreeCopyHandler.EP_NAME, classOf[TreeCopyHandler])
    CoreApplicationEnvironment.registerExtensionPoint(area, "com.intellij.multiverseEnabler", classOf[com.intellij.codeInsight.multiverse.MultiverseEnabler])

    appServices.register(classOf[ProgressManager])(new CoreProgressManager)
    appServices.register(classOf[PsiBuilderFactory])(new PsiBuilderFactoryImpl)
    appServices.register(classOf[DefaultASTFactory])(new DefaultASTFactoryImpl)
    // In-memory documents; in the LSP host these become the client's snapshots.
    appServices.register(classOf[com.intellij.openapi.fileEditor.FileDocumentManager])(
      new com.intellij.mock.MockFileDocumentManagerImpl(null, (text: CharSequence) => new com.intellij.openapi.editor.impl.DocumentImpl(text)))
    // PSI writes check command state; the core processor tracks it without UI or undo.
    appServices.register(classOf[com.intellij.openapi.command.CommandProcessor])(new com.intellij.openapi.command.impl.CoreCommandProcessor)
    // The asynchronous document commit pipeline: the host commits synchronously and provides none.
    appServices.registerNull(classOf[com.intellij.psi.impl.DocumentCommitProcessor])
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
    appServices.register(classOf[com.intellij.psi.codeStyle.CodeStyleSettingsService])(
      tableProxy(classOf[com.intellij.psi.codeStyle.CodeStyleSettingsService], "CodeStyleSettingsService") {
        case ("addListener", _) => null
        case ("getFileTypeIndentOptionsFactories" | "getLanguageCodeStyleProviders", _) => java.util.List.of()
        // Only Scala's custom settings bean, with defaults; not the settings-UI provider.
        case ("getCustomCodeStyleSettingsFactories", _) =>
          java.util.List.of(new com.intellij.psi.codeStyle.CustomCodeStyleSettingsFactory {
            override def createCustomSettings(s: com.intellij.psi.codeStyle.CodeStyleSettings) =
              new org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings(s)
          })
      })
    val libraryIndex = new LibraryIndex
    appServices.register(classOf[com.intellij.psi.stubs.StubIndex])(libraryIndex.stubIndex)
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
    appServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.api.expr.ExpectedTypes])(
      new org.jetbrains.plugins.scala.lang.psi.impl.expr.ExpectedTypesImpl)
    appServices.register(classOf[org.jetbrains.plugins.scala.scalaMeta.ScalaMetaApi])(new scala.meta.intellij.ScalaMetaApiImpl)

    // ---- 2. Language registries and stub schema: explicit extensions only ---------------------
    val parserDefinitions: Seq[ParserDefinition] = Seq(
      new org.jetbrains.plugins.scala.lang.parser.ScalaParserDefinition,
      new org.jetbrains.plugins.scala.lang.parser.Scala3ParserDefinition,
      new org.jetbrains.plugins.scalaDoc.lang.parser.ScalaDocParserDefinition,
      new org.jetbrains.plugins.scalaDirective.lang.parser.ScalaDirectiveParserDefinition,
    )
    parserDefinitions.foreach(pd => LanguageParserDefinitions.INSTANCE.addExplicitExtension(pd.getFileNodeType.getLanguage, pd))
    val scalaLang = org.jetbrains.plugins.scala.ScalaLanguage.INSTANCE
    val scala3Lang = org.jetbrains.plugins.scala.Scala3Language.INSTANCE
    LanguageASTFactory.INSTANCE.addExplicitExtension(scalaLang, new org.jetbrains.plugins.scala.lang.parser.ScalaASTFactory)
    val viewProviderFactory = new org.jetbrains.plugins.scala.lang.psi.ScFileViewProviderFactory
    LanguageFileViewProviders.INSTANCE.addExplicitExtension(scalaLang, viewProviderFactory)
    LanguageFileViewProviders.INSTANCE.addExplicitExtension(scala3Lang, viewProviderFactory)

    // Stub schema (model): the plugin's serializer/factory registrations and language stub definitions.
    val scalaPlugin = new DefaultPluginDescriptor(PluginId.getId("org.intellij.scala"))
    declare("com.intellij.stubElementRegistryExtension").asInstanceOf[ExtensionPointImpl[StubRegistryExtension]]
      .registerExtension(new org.jetbrains.plugins.scala.lang.psi.stubs.ScalaStubRegistryExtension, root)
    val stubDefinitions = declare("com.intellij.languageStubDefinition")
      .asInstanceOf[ExtensionPointImpl[LanguageExtensionPoint[LanguageStubDefinition]]]
    def stubDefinition(language: String, definition: LanguageStubDefinition) = {
      val bean = new LanguageExtensionPoint[LanguageStubDefinition](language, definition)
      bean.setPluginDescriptor(scalaPlugin)
      bean
    }
    stubDefinitions.registerExtension(stubDefinition("Scala", new org.jetbrains.plugins.scala.lang.psi.stubs.ScalaLanguageStubDefinition), scalaPlugin, root)
    stubDefinitions.registerExtension(stubDefinition("Scala 3", new org.jetbrains.plugins.scala.lang.psi.stubs.Scala3LanguageStubDefinition), scalaPlugin, root)
    appServices.register(classOf[StubElementRegistryService])(
      new com.intellij.psi.stubs.StubElementRegistryServiceImpl(kotlinx.coroutines.CoroutineScopeKt.CoroutineScope(kotlin.coroutines.EmptyCoroutineContext.INSTANCE)))

    Registry.Companion.setContributedKeys(Descriptors.scalaRegistryKeys(getClass.getClassLoader).asJava)
    Registry.Companion.markAsLoaded()

    val fileTypes = new CoreFileTypeRegistry
    fileTypes.registerFileType(org.jetbrains.plugins.scala.ScalaFileType.INSTANCE, "scala")
    FileTypeRegistry.setInstanceSupplier(() => fileTypes, root)

    // ---- 3. Project object -----------------------------------------------------------------
    val psiModTracker: PsiModificationTracker = {
      val counter = new SimpleModificationTracker
      tableProxy(classOf[PsiModificationTracker], "PsiModificationTracker") {
        case ("getModificationCount", _) => java.lang.Long.valueOf(counter.getModificationCount)
        case ("getForLanguage" | "forLanguage", _) => counter
      }
    }
    lazy val projectBus = new MessageBusFactoryImpl().createMessageBus(busOwner, appBus)
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
    // The host's snapshot store: view providers keyed by (light) virtual file.
    val viewProviders = new ConcurrentHashMap[VirtualFile, FileViewProvider]()
    val fileManager = tableProxy(classOf[com.intellij.psi.impl.file.impl.FileManager], "FileManager") {
      case ("findCachedViewProvider", Array(f: VirtualFile)) => viewProviders.get(f)
      case ("setViewProvider", Array(f: VirtualFile, vp)) =>
        if (vp == null) viewProviders.remove(f) else viewProviders.put(f, vp.asInstanceOf[FileViewProvider]); null
    }
    val fileIndex = tableProxy(classOf[com.intellij.openapi.roots.ProjectFileIndex], "ProjectFileIndex") {
      // No module model yet: language features are attached per file instead.
      case ("isInLibrary" | "isInLibraryClasses" | "isInLibrarySource", _) => java.lang.Boolean.FALSE
      case ("getModuleForFile", _) => null
      // Every file the host knows is a source file it was given.
      case ("isInSourceContent" | "isInContent" | "isInSource", _) => java.lang.Boolean.TRUE
      case ("isInTestSourceContent", _) => java.lang.Boolean.FALSE
    }
    projectServices.register(classOf[com.intellij.openapi.roots.ProjectFileIndex])(fileIndex)
    val psiManager = new HostPsiManager(project, psiModTracker, fileManager)
    projectServices.register(classOf[PsiManager])(psiManager)
    projectServices.register(classOf[PsiModificationTracker])(psiModTracker)
    // PSI <-> document synchronization: the core implementation used by headless compilers (package-private).
    projectServices.register(classOf[com.intellij.psi.PsiDocumentManager])(
      newPrivate(Class.forName("com.intellij.core.CorePsiDocumentManager"), project).asInstanceOf[com.intellij.psi.PsiDocumentManager])
    // Platform library implementations, constructed by the host (no container wiring).
    projectServices.register(classOf[com.intellij.psi.util.CachedValuesManager])(
      new com.intellij.util.CachedValuesManagerImpl(project, new com.intellij.psi.impl.PsiCachedValuesFactory(project)))
    projectServices.register(classOf[com.intellij.psi.PsiFileFactory])(new com.intellij.psi.impl.PsiFileFactoryImpl(psiManager))
    projectServices.register(classOf[com.intellij.openapi.roots.ProjectRootManager])(new Host_ProjectRootManager(dispatch {
      case ("ProjectRootManager.getFileIndex", _) => fileIndex
    }))
    // The host's indexes are always ready before it answers queries.
    projectServices.register(classOf[DumbService])(new Host_DumbService(dispatch {
      case ("DumbService.isDumb", _) => java.lang.Boolean.FALSE
      case ("DumbService.getProject", _) => project
      // Dumb-mode tasks (synthetic class registration) run to completion on the caller's thread.
      case ("DumbService.queueTask", Array(task: DumbModeTask)) =>
        try task.performInDumbMode(new EmptyProgressIndicator(ModalityState.nonModal()))
        finally Disposer.dispose(task)
        null
    }))
    // No module model yet.
    projectServices.register(classOf[com.intellij.openapi.module.ModuleManager])(new Host_ModuleManager(dispatch {
      case ("ModuleManager.getModules", _) => Array.empty[com.intellij.openapi.module.Module]
    }))
    // PSI write path: the plugin edits non-physical copies (e.g. for-comprehension desugaring).
    projectServices.register(classOf[com.intellij.pom.PomModel])(new com.intellij.pom.core.impl.PomModelImpl(project))
    projectServices.register(classOf[com.intellij.pom.tree.TreeAspect])(new com.intellij.pom.tree.TreeAspect)
    projectServices.register(classOf[com.intellij.psi.SmartPointerManager])(new com.intellij.psi.impl.smartPointers.SmartPointerManagerImpl(project))
    lazy val javaElementFactory = new com.intellij.psi.impl.PsiElementFactoryImpl(project)
    projectServices.register(classOf[com.intellij.psi.search.ProjectScopeBuilder])(new Host_ProjectScopeBuilder(dispatch {
      case (m, _) if m.startsWith("ProjectScopeBuilder.build") => new com.intellij.psi.search.EverythingGlobalScope(project)
    }))
    // Java/classpath symbols are not provided yet (phase F1): every query is recorded as a known gap.
    projectServices.register(classOf[com.intellij.psi.JavaPsiFacade])(new Host_JavaPsiFacade(dispatch {
      case ("JavaPsiFacade.getProject", _) => project
      case ("JavaPsiFacade.getElementFactory", _) => javaElementFactory
      case ("JavaPsiFacade.findClass", args) => CallLog.hit(s"GAP classpath findClass ${args(0)}"); null
      case ("JavaPsiFacade.findClasses", args) => CallLog.hit(s"GAP classpath findClasses ${args(0)}"); Array.empty[com.intellij.psi.PsiClass]
      case ("JavaPsiFacade.findPackage", args) => CallLog.hit(s"GAP classpath findPackage ${args(0)}"); null
    }))
    // ScalaDocParserDefinition reads TAB_SIZE from code style: one fixed settings object, never the scheme manager.
    projectServices.register(classOf[com.intellij.psi.codeStyle.ProjectCodeStyleSettingsManager]) {
      val m = new com.intellij.psi.codeStyle.ProjectCodeStyleSettingsManager(project, /* loadExtensions = */ false)
      m.USE_PER_PROJECT_SETTINGS = true
      m
    }
    projectServices.register(classOf[com.intellij.psi.impl.ResolveScopeManager])(new Host_ResolveScopeManager(dispatch {
      // One scope for everything until build targets exist (phase F2).
      case ("ResolveScopeManager.getResolveScope" | "ResolveScopeManager.getDefaultResolveScope" | "ResolveScopeManager.getUseScope", _) =>
        new com.intellij.psi.search.EverythingGlobalScope(project)
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
      settings
    }
    projectServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager])(
      new org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager(using project))
    projectServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes])(
      new org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes(using new ProjectContext(project)))
    projectServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.SyntheticClasses])(
      new org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.SyntheticClasses(project))
    projectServices.register(classOf[org.jetbrains.plugins.scala.lang.macros.evaluator.ScalaMacroEvaluator])(
      new org.jetbrains.plugins.scala.lang.macros.evaluator.ScalaMacroEvaluator(project))
    // Private light services (@Service): the container builds them by class name, and so does the host.
    Seq(
      "org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType$SubstitutorCacheService",
      "org.jetbrains.plugins.scala.util.UnloadAwareDisposable$UnloadAwareDisposableService",
    ).foreach { name =>
      val cls = Class.forName(name).asInstanceOf[Class[AnyRef]]
      projectServices.register(cls)(newPrivate(cls))
    }

    // ---- 4. Project startup the PSI depends on -------------------------------------------------
    // The plugin's own activity registers the synthetic classes (Any, Int, ...) through DumbService.queueTask.
    new org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.RegisterSyntheticClassesProjectActivity().execute(project)

    new ScalaPsiHost(project, libraryIndex)
  }

  private def newPrivate[T](cls: Class[T], args: AnyRef*): T = {
    val ctor = cls.getDeclaredConstructors.find(_.getParameterCount == args.size)
      .getOrElse(throw new IllegalStateException(s"no ${args.size}-argument constructor in ${cls.getName}"))
    ctor.setAccessible(true)
    cls.cast(ctor.newInstance(args*))
  }
}
