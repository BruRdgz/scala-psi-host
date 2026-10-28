package scalapsihost

import com.intellij.codeInsight.daemon.impl.{AnnotationHolderImpl, AnnotationSessionImpl}
import com.intellij.core.{CoreApplicationEnvironment, CoreFileTypeRegistry}
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiErrorElement, PsiFile, PsiJavaFile, SyntaxTraverser}
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
  val project: Project,
  symbolIndex: SymbolIndex,
  libraryFiles: LibraryFileSystem,
  val modules: ModuleModel,
  psiChanged: () => Unit,
) {
  // Library jars by real path: a jar on the classpath of several modules is mounted and indexed once.
  private val libraryRoots = new ConcurrentHashMap[Path, VirtualFile]()

  /** A file outside the project: parsed, but invisible to symbol lookups (as the IDE's non-physical files). */
  def parse(text: String, features: ScalaFeatures = ScalaFeatures.default): ScalaFile =
    ScalaPsiElementFactory.createScalaFileFromText(text, features)(using new ProjectContext(project))

  /**
   * A project source file: its declarations are indexed, so other files and the file itself find them by name,
   * as for a file under an IDE source root. Fails when its stubs cannot be paired with its PSI.
   */
  def openSource(text: String, features: ScalaFeatures = ScalaFeatures.default): ScalaFile =
    index(parse(text, features))

  /**
   * A source file of `module`: parsed with the module's language features (Scala version, compiler options) and
   * resolved in the module's scope.
   */
  def openSource(text: String, module: HostModule): ScalaFile =
    openInModule(parse(text, org.jetbrains.plugins.scala.project.ModuleExt(module.module).features), module)

  /**
   * A Java source file of `module` (mixed Scala/Java modules): its classes are indexed, so the module's Scala code
   * resolves them. `fileName` is the file's name (`Foo.java`), which Java ties to its public class.
   */
  def openJavaSource(fileName: String, text: String, module: HostModule): PsiJavaFile = {
    val file = com.intellij.psi.PsiFileFactory.getInstance(project)
      .createFileFromText(fileName, com.intellij.lang.java.JavaLanguage.INSTANCE, text)
      .asInstanceOf[PsiJavaFile]
    openInModule(file, module)
  }

  private def openInModule[F <: PsiFile](file: F, module: HostModule): F = {
    // The file is non-physical (no PsiFile.getVirtualFile): the platform finds its module through this key, as for
    // code fragments.
    file.putUserData(com.intellij.openapi.module.ModuleUtilCore.KEY_MODULE, module.module)
    modules.addSource(file.getViewProvider.getVirtualFile, module)
    index(file)
  }

  private def index[F <: PsiFile](file: F): F = {
    if (!symbolIndex.indexSourceFile(file)) {
      modules.removeSource(file.getViewProvider.getVirtualFile)
      throw new IllegalStateException("source file stubs could not be paired with its PSI")
    }
    psiChanged()
    file
  }

  /** Removes a source file opened with [[openSource]] from the project. */
  def closeSource(file: PsiFile): Unit = {
    symbolIndex.removeFile(file)
    modules.removeSource(file.getViewProvider.getVirtualFile)
    psiChanged()
  }

  /** Mounts a library jar and indexes its Scala class files accepted by `includeEntry` (e.g. `scala/Option.class`). */
  def addLibrary(jar: Path, includeEntry: String => Boolean = _ => true): LibraryIndexReport = {
    val root = libraryFiles.mountJar(jar)
    libraryRoots.put(jar.toRealPath(), root)
    val report = symbolIndex.indexLibrary(project, root, includeEntry)
    psiChanged()
    report
  }

  /** Mounts the JDK's modules. Java classes are read on demand, not indexed. Every module sees the JDK. */
  def addJdk(javaHome: Path): Unit = {
    modules.setJdkRoots(libraryFiles.mountJdk(javaHome))
    psiChanged()
  }

  /**
   * Adds a module as an IDE module after a build import: the jars of its classpath are mounted and indexed (each
   * jar once, however many modules use it), its Scala version becomes a Scala SDK library and its compiler options
   * a compiler-settings profile, from which the plugin derives the module's language features.
   * Classpath entries must be jars; the class directories of other modules are given by `dependsOn`.
   */
  def addModule(spec: ModuleSpec): HostModule = {
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
  def isCompilerHighlighted(file: ScalaFile): Boolean =
    org.jetbrains.plugins.scala.settings.ScalaHighlightingMode.isShowErrorsFromCompilerEnabled(file)

  /**
   * The problems the IDE's highlighting pass reports for `file`: syntax errors, and the plugin's `ScalaAnnotator`
   * run over every element through the platform's annotation session. Presentation-only annotations (no message)
   * are left out.
   */
  def diagnostics(file: ScalaFile): Seq[Diagnostic] = {
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
}

final case class Diagnostic(severity: HighlightSeverity, range: TextRange, message: String)

object ScalaPsiHost {
  @volatile private var started: Option[ScalaPsiHost] = None

  /** Plugin extension points that carry language semantics; every other plugin extension point stays empty. */
  val SemanticExtensionPoints: Seq[String] = Seq("org.intellij.scala.syntheticMemberInjector")

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
      // The host has no UI thread and no write actions: every caller may read.
      case ("isDispatchThread", _) => java.lang.Boolean.FALSE
      case ("isWriteIntentLockAcquired", _) => java.lang.Boolean.FALSE
      case ("isReadAccessAllowed", _) => java.lang.Boolean.TRUE
      case ("isWriteAccessAllowed", _) => java.lang.Boolean.FALSE
      case ("assertReadAccessAllowed" | "assertIsNonDispatchThread" | "assertReadAccessNotAllowed", _) => null
      case ("isUnitTestMode", _) => java.lang.Boolean.valueOf(config.unitTestMode)
      case ("isInternal" | "isEAP" | "isDisposed", _) => java.lang.Boolean.FALSE
      case ("isHeadlessEnvironment" | "isCommandLine", _) => java.lang.Boolean.TRUE
      case ("getUserData", Array(k: Key[?])) => appData.getUserData(k).asInstanceOf[AnyRef]
      // Extension points resolve their declared class through the component manager.
      case ("loadClass", Array(name: String, _)) => Class.forName(name)
      // Wraps an extension's failure for logging; the IDE adds plugin attribution, which the host does not track.
      case ("createError", Array(t: Throwable, _*)) => new RuntimeException(t)
      case ("runReadAction", Array(r: Runnable)) => r.run(); null
      case ("tryRunReadAction", Array(r: Runnable)) => r.run(); java.lang.Boolean.TRUE
      // In unit-test mode the plugin runs some startup work on the UI thread; the host has none, so it runs here.
      case ("invokeAndWait", Array(r: Runnable, _*)) if config.unitTestMode => r.run(); null
      case ("runReadAction", Array(c: com.intellij.openapi.util.Computable[?])) => c.compute().asInstanceOf[AnyRef]
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
    // Languages that stand for groups of languages in language extensions; none in the host.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.lang.MetaLanguage.EP_NAME, classOf[com.intellij.lang.MetaLanguage])
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
    appServices.register(classOf[org.jetbrains.plugins.scala.lang.psi.api.expr.ExpectedTypes])(
      new org.jetbrains.plugins.scala.lang.psi.impl.expr.ExpectedTypesImpl)
    appServices.register(classOf[org.jetbrains.plugins.scala.scalaMeta.ScalaMetaApi])(new scala.meta.intellij.ScalaMetaApiImpl)
    // The editor's read/write-action facade, reached from CodeStyle.getSettings: the host's callers may always read.
    appServices.register(classOf[com.intellij.openapi.editor.elf.Elf])(tableProxy(classOf[com.intellij.openapi.editor.elf.Elf], "Elf") {
      case ("runReadAction", Array(f: kotlin.jvm.functions.Function0[?])) => f.invoke().asInstanceOf[AnyRef]
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
    // Annotations carry their quick fixes, which the platform wraps as intentions when the annotation is created.
    appServices.register(classOf[com.intellij.modcommand.ModCommandService])(new com.intellij.lang.impl.modcommand.ModCommandServiceImpl)
    // Platform library implementations used by Java PSI of library classes and by PSI copies.
    appServices.register(classOf[com.intellij.psi.util.JavaClassSupers])(new com.intellij.psi.impl.JavaClassSupersImpl)
    appServices.register(classOf[com.intellij.psi.PsiSubstitutorFactory])(new com.intellij.psi.impl.PsiSubstitutorFactoryImpl)
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
    // Java sources of mixed projects.
    LanguageParserDefinitions.INSTANCE.addExplicitExtension(com.intellij.lang.java.JavaLanguage.INSTANCE, new com.intellij.lang.java.JavaParserDefinition)
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
    FileTypeRegistry.setInstanceSupplier(() => fileTypes, root)

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
    val libraryFiles = new LibraryFileSystem
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
    val psiModificationCount = new SimpleModificationTracker
    val psiModTracker: PsiModificationTracker = tableProxy(classOf[PsiModificationTracker], "PsiModificationTracker") {
      case ("getModificationCount", _) => java.lang.Long.valueOf(psiModificationCount.getModificationCount)
      case ("getForLanguage" | "forLanguage", _) => psiModificationCount
    }
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
    // The host's file store: view providers of light files (snapshots) and of library class files, keyed by
    // virtual file; light files register themselves, library class files are created on first request.
    val viewProviders = new ConcurrentHashMap[VirtualFile, FileViewProvider]()
    val directories = new ConcurrentHashMap[VirtualFile, com.intellij.psi.PsiDirectory]()
    var psiManagerRef: PsiManager = null
    def viewProvider(f: VirtualFile): FileViewProvider = viewProviders.computeIfAbsent(f, {
      case lib: LibraryFile if com.intellij.psi.FileTypeFileViewProviders.INSTANCE.forFileType(lib.getFileType) != null =>
        com.intellij.psi.FileTypeFileViewProviders.INSTANCE.forFileType(lib.getFileType).createFileViewProvider(lib, null, psiManagerRef, false)
      case other => CallLog.unsupported(s"FileManager.findViewProvider for a file the host does not own: $other")
    })
    val fileManager = tableProxy(classOf[com.intellij.psi.impl.file.impl.FileManager], "FileManager") {
      case ("findCachedViewProvider", Array(f: VirtualFile)) => viewProviders.get(f)
      case ("setViewProvider", Array(f: VirtualFile, vp)) =>
        if (vp == null) viewProviders.remove(f) else viewProviders.put(f, vp.asInstanceOf[FileViewProvider]); null
      case ("findViewProvider", Array(f: VirtualFile)) => viewProvider(f)
      case ("findFile", Array(f: VirtualFile)) =>
        if (f.isDirectory) null else { val vp = viewProvider(f); vp.getPsi(vp.getBaseLanguage) }
      case ("findDirectory", Array(f: VirtualFile)) =>
        if (!f.isDirectory) null
        else directories.computeIfAbsent(f, d => new com.intellij.psi.impl.file.PsiDirectoryImpl(psiManagerRef.asInstanceOf[com.intellij.psi.impl.PsiManagerEx], d))
    }
    // Library class files live in the host's library file system; every other file is a source the host was given.
    def isLibraryClass(f: VirtualFile) = f.getFileSystem.isInstanceOf[LibraryFileSystem]
    val fileIndex = tableProxy(classOf[com.intellij.openapi.roots.ProjectFileIndex], "ProjectFileIndex") {
      case ("isInLibrary" | "isInLibraryClasses", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(isLibraryClass(f))
      // No library sources are attached yet, so no workspace library or SDK entity has any to offer.
      case ("isInLibrarySource", _) => java.lang.Boolean.FALSE
      case ("findContainingLibraries" | "findContainingSdks", _) => java.util.List.of()
      // Files opened in a module; other files (light files, libraries) have none.
      case ("getModuleForFile", Array(f: VirtualFile, _*)) => moduleModel.ofFile(f).map(_.module).orNull
      case ("isInSourceContent" | "isInContent" | "isInSource", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(!isLibraryClass(f))
      case ("isInTestSourceContent", _) => java.lang.Boolean.FALSE
      // Every source file the host was given is a production source; the plugin's filter scopes ask by root type.
      case ("isUnderSourceRootOfType", Array(f: VirtualFile, types: java.util.Set[?])) =>
        java.lang.Boolean.valueOf(!isLibraryClass(f) && types.contains(org.jetbrains.jps.model.java.JavaSourceRootType.SOURCE))
    }
    projectServices.register(classOf[com.intellij.openapi.roots.ProjectFileIndex])(fileIndex)
    // The same answers through the platform's facade (ClassFileViewProvider asks it before building Java PSI).
    projectServices.register(classOf[com.intellij.openapi.roots.FileIndexFacade])(new Host_FileIndexFacade(project, dispatch {
      case ("FileIndexFacade.isInLibraryClasses" | "FileIndexFacade.isInLibrary", Array(f: VirtualFile)) => java.lang.Boolean.valueOf(isLibraryClass(f))
      case ("FileIndexFacade.isInLibrarySource", _) => java.lang.Boolean.FALSE
      // Roots change only when a library or the JDK is mounted, which also counts as a PSI change.
      case ("FileIndexFacade.getRootModificationTracker", _) => psiModificationCount
      case ("FileIndexFacade.isInSource" | "FileIndexFacade.isInSourceContent" | "FileIndexFacade.isInContent", Array(f: VirtualFile)) =>
        java.lang.Boolean.valueOf(!isLibraryClass(f))
    }))
    val psiManager = new HostPsiManager(project, psiModTracker, fileManager)
    psiManagerRef = psiManager
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
    projectServices.register(classOf[com.intellij.pom.PomModel])(new com.intellij.pom.core.impl.PomModelImpl(project))
    projectServices.register(classOf[com.intellij.pom.tree.TreeAspect])(new com.intellij.pom.tree.TreeAspect)
    projectServices.register(classOf[com.intellij.psi.SmartPointerManager])(new com.intellij.psi.impl.smartPointers.SmartPointerManagerImpl(project))
    projectServices.register(classOf[com.intellij.psi.PsiElementFactory])(new com.intellij.psi.impl.PsiElementFactoryImpl(project))
    projectServices.register(classOf[com.intellij.psi.search.ProjectScopeBuilder])(new Host_ProjectScopeBuilder(dispatch {
      case (m, _) if m.startsWith("ProjectScopeBuilder.build") => new com.intellij.psi.search.EverythingGlobalScope(project)
    }))
    // Class and package lookup: the platform's JavaPsiFacadeImpl over element finders, as in the IDE. The plugin's
    // finders come with the host's LibraryClassFinder, which takes the place of the IDE's PsiElementFinderImpl.
    projectServices.register(classOf[com.intellij.psi.JvmPsiConversionHelper])(new com.intellij.psi.impl.JvmPsiConversionHelperImpl)
    projectServices.register(classOf[com.intellij.psi.impl.source.resolve.ResolveCache])(new com.intellij.psi.impl.source.resolve.ResolveCache(project))
    // External (XML) annotations of library classes: the host attaches none.
    projectServices.register(classOf[com.intellij.codeInsight.ExternalAnnotationsManager])(new Host_ExternalAnnotationsManager(dispatch {
      case ("ExternalAnnotationsManager.findExternalAnnotationsFiles", _) => null
    }))
    // Nullability annotations of library Java types: the host configures none (the IDE's defaults come from settings).
    projectServices.register(classOf[com.intellij.codeInsight.NullableNotNullManager])(new Host_NullableNotNullManager(project, dispatch {
      case ("NullableNotNullManager.getNullityDefault", _) => com.intellij.codeInsight.ContextNullabilityInfo.EMPTY
      case ("NullableNotNullManager.findEffectiveNullabilityInfo", _) => null
    }))
    // Java class files navigate to their sources through this helper; the host attaches none, so a class file is its
    // own navigation element, as in the IDE for a library without sources.
    projectServices.register(classOf[com.intellij.psi.impl.JavaPsiImplementationHelper])(new Host_JavaPsiImplementationHelper(dispatch {
      case ("JavaPsiImplementationHelper.getClsFileNavigationElement", Array(f: com.intellij.psi.PsiJavaFile)) => f
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
    // Java classes by short name, which auto-import fixes combine with the plugin's Scala index. The host has no
    // Java short-name index yet (Java class files are read by qualified name): no Java class is offered.
    projectServices.register(classOf[com.intellij.psi.search.PsiShortNamesCache])(new Host_PsiShortNamesCache(dispatch {
      case ("PsiShortNamesCache.getClassesByName", _) => com.intellij.psi.PsiClass.EMPTY_ARRAY
    }))
    // Auto-import exclusions, read by quick fixes that look for importable implicit instances: the defaults.
    projectServices.register(classOf[com.intellij.codeInsight.JavaProjectCodeInsightSettings])(new com.intellij.codeInsight.JavaProjectCodeInsightSettings)
    // No language injection in the host: every file is its own top-level file.
    projectServices.register(classOf[com.intellij.lang.injection.InjectedLanguageManager])(new Host_InjectedLanguageManager(dispatch {
      case ("InjectedLanguageManager.getTopLevelFile", Array(e: com.intellij.psi.PsiElement)) => e.getContainingFile
      case ("InjectedLanguageManager.isInjectedViewProvider", _) => java.lang.Boolean.FALSE
    }))
    // JVM-language-neutral class lookup that JavaPsiFacadeImpl also consults; no other JVM language provides classes.
    CoreApplicationEnvironment.registerExtensionPoint(area, com.intellij.lang.jvm.facade.JvmElementProvider.EP_NAME.getName,
      classOf[com.intellij.lang.jvm.facade.JvmElementProvider])
    projectServices.register(classOf[com.intellij.lang.jvm.facade.JvmFacade])(new com.intellij.lang.jvm.facade.JvmFacadeImpl(project))
    projectServices.register(classOf[com.intellij.psi.impl.file.impl.JavaFileManager])(
      tableProxy(classOf[com.intellij.psi.impl.file.impl.JavaFileManager], "JavaFileManager")(PartialFunction.empty))
    projectServices.register(classOf[com.intellij.psi.JavaPsiFacade])(new com.intellij.psi.impl.JavaPsiFacadeImpl(project, null))
    // ScalaDocParserDefinition reads TAB_SIZE from code style: one fixed settings object, never the scheme manager.
    projectServices.register(classOf[com.intellij.psi.codeStyle.ProjectCodeStyleSettingsManager]) {
      val m = new com.intellij.psi.codeStyle.ProjectCodeStyleSettingsManager(project, /* loadExtensions = */ false)
      m.USE_PER_PROJECT_SETTINGS = true
      m
    }
    projectServices.register(classOf[com.intellij.psi.impl.ResolveScopeManager])(new Host_ResolveScopeManager(dispatch {
      // A module's files resolve in the module's scope; files outside modules (light files, library classes) see
      // everything.
      case ("ResolveScopeManager.getResolveScope" | "ResolveScopeManager.getUseScope", Array(e: com.intellij.psi.PsiElement)) =>
        val file = Option(e.getContainingFile).map(_.getOriginalFile.getViewProvider.getVirtualFile)
        file.flatMap(moduleModel.ofFile).map(moduleModel.resolveScope).getOrElse(new com.intellij.psi.search.EverythingGlobalScope(project))
      case ("ResolveScopeManager.getDefaultResolveScope", Array(f: VirtualFile)) =>
        moduleModel.ofFile(f).map(moduleModel.resolveScope).getOrElse(new com.intellij.psi.search.EverythingGlobalScope(project))
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
    def psiChanged(): Unit = {
      psiModificationCount.incModificationCount()
      projectBus.syncPublisher(PsiModificationTracker.TOPIC).modificationCountChanged()
      org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager.instance(project).clearOnNonScalaChange()
    }

    new ScalaPsiHost(project, symbolIndex, libraryFiles, moduleModel, () => psiChanged())
  }

  private def newPrivate[T](cls: Class[T], args: AnyRef*): T = {
    val ctor = cls.getDeclaredConstructors.find(_.getParameterCount == args.size)
      .getOrElse(throw new IllegalStateException(s"no ${args.size}-argument constructor in ${cls.getName}"))
    ctor.setAccessible(true)
    cls.cast(ctor.newInstance(args*))
  }
}
