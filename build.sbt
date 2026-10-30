// The Scala plugin and platform 263.x are class file 69: compiling against them (javac) and running them needs JDK 25.
val requiredJavaVersion = 25

val platformVersion = "263.7307.44"

// The intellij-scala revision the host is built and tested against (branch idea263.x).
val scalaPluginRevision = "491569803c6c3c2f93a0bda51490425fda237fc0"

// The Scala plugin's own jars, built from intellij-scala with `sbt packageArtifactZip`.
val scalaPluginLib = settingKey[File]("Directory with the Scala plugin's jars (Scala/lib)")

// Library binaries the tests index. Kept out of the compile/test classpath on purpose.
val Fixtures = config("fixtures").hide
// A second configuration: two versions of one module in a single configuration would evict each other.
val Fixtures212 = config("fixtures212").hide
// Downloaded only as input to the slice task, never a compile or runtime dependency.
val InlayPlatform = config("inlay-platform").hide
val inlayPlatformJar = taskKey[File]("The platform classes used by Scala hints and word selection")

val scalaPluginJars = taskKey[Seq[File]]("The Scala plugin's jars that go on the classpath")

val dist = taskKey[File]("Builds the language server's distribution: bin/scala-psi-host and lib/ in target/dist")

ThisBuild / scalaVersion := "3.9.0"
// The compiler's import suggestions (on a "not found" error) load every class of the classpath's packages; some
// platform class files refer to IDE classes that are not on it, and the compiler crashes instead of reporting the error.
ThisBuild / scalacOptions += "-Ximport-suggestion-timeout:0"

lazy val root = (project in file("."))
  .configs(Fixtures, Fixtures212, InlayPlatform)
  .settings(
    name := "scala-psi-host",
    initialize := {
      val _ = initialize.value
      val running = java.lang.Runtime.version()
      if (running.feature() < requiredJavaVersion)
        sys.error(s"scala-psi-host needs sbt to run on JDK $requiredJavaVersion+ (found $running); use `sbt --java-home <jdk25>`")
    },
    scalaPluginLib := (sys.env.get("SCALA_PLUGIN_LIB") match {
      // A build of the plugin from elsewhere: its revision is the responsibility of whoever points to it.
      case Some(lib) => file(lib)
      case None =>
        val checkout = file("../intellij-scala")
        val head = scala.util.Try(scala.sys.process.Process(Seq("git", "-C", checkout.getPath, "rev-parse", "HEAD")).!!.trim)
        if (!head.toOption.contains(scalaPluginRevision))
          sys.error(s"$checkout must be at intellij-scala $scalaPluginRevision (found ${head.getOrElse("no git checkout")}); " +
            "check it out and build the plugin there, or set SCALA_PLUGIN_LIB")
        checkout / "target" / "plugin" / "Scala" / "lib"
    }).getCanonicalFile,
    scalaPluginJars := {
      val lib = scalaPluginLib.value
      if (!lib.isDirectory) sys.error(s"Scala plugin jars not found at $lib (set SCALA_PLUGIN_LIB)")
      // The plugin bundles its own Scala runtime; this build provides one.
      val main = (lib * "*.jar").get.filterNot(f => Set("scala-library.jar", "scala3-library_3.jar")(f.getName))
      // Content modules the host uses: the structure view (document symbols).
      val modules = Seq("scalaCommunity.structure-view.jar").map(lib / "modules" / _)
      modules.filterNot(_.isFile).foreach(m => sys.error(s"Scala plugin module jar not found: $m"))
      main ++ modules
    },
    resolvers ++= Seq(
      "intellij-snapshots" at "https://www.jetbrains.com/intellij-repository/snapshots",
      "intellij-dependencies" at "https://cache-redirector.jetbrains.com/intellij-dependencies",
    ),
    libraryDependencies ++= Seq(
      "com.jetbrains.intellij.platform" % "ide-impl" % platformVersion % InlayPlatform intransitive(),
      "com.jetbrains.intellij.platform" % "core-impl" % platformVersion,
      "com.jetbrains.intellij.platform" % "analysis-impl" % platformVersion,
      "com.jetbrains.intellij.java" % "java-psi-impl" % platformVersion,
      "com.jetbrains.intellij.platform" % "indexing-impl" % platformVersion,
      // IndentHelperImpl: PSI copies record their indentation (ChangeUtil.copyElement). Only this jar, not the formatter.
      "com.jetbrains.intellij.platform" % "code-style-impl" % platformVersion intransitive(),
      // Java stub factories and serializers (JavaStubRegistryExtension): the stub schema of Java class files.
      "com.jetbrains.intellij.java" % "java-indexing-serializers" % platformVersion,
      // PsiShortNamesCache, the Java short-name lookup auto-import fixes query. Only this jar.
      "com.jetbrains.intellij.java" % "java-indexing" % platformVersion intransitive(),
      // JavaStubDefinition: the stub definition of Java source files (project sources in mixed projects). Only this jar.
      "com.jetbrains.intellij.java" % "java-indexing-impl" % platformVersion intransitive(),
      // Class-member model (PsiElementClassMember, ClassMemberWithElement): the plugin's members-to-implement
      // check, behind the "class needs to be abstract" error. Only these jars; nothing in them is registered.
      "com.jetbrains.intellij.java" % "java-impl" % platformVersion intransitive(),
      "com.jetbrains.intellij.platform" % "lang-impl" % platformVersion intransitive(),
      "com.jetbrains.intellij.platform" % "lang" % platformVersion intransitive(),
      // Java's highlighting keys (JavaHighlightInfoTypes), which the plugin's Scala highlighting keys derive from; quick
      // documentation (hover) renders signatures with them. Only these jars (and their message bundle); the host has
      // no colors.
      "com.jetbrains.intellij.java" % "java-analysis-impl" % platformVersion intransitive(),
      "com.jetbrains.intellij.java" % "java-analysis" % platformVersion intransitive(),
      // External-system data model (model.Key, model.project.*): the plugin's sbt constants, read when it checks for sbt build
      // modules. Only this jar; no external system runs.
      "com.jetbrains.intellij.platform" % "external-system" % platformVersion intransitive(),
      // AccessLevelProvider, which the plugin's structure-view elements implement (document symbols). Only this jar.
      "com.jetbrains.intellij.java" % "java-structure-view" % platformVersion intransitive(),
      "com.jetbrains.intellij.platform" % "external-system-rt" % platformVersion intransitive(),
      // XML PSI: ScalaDoc and XML literals parse through it.
      "com.jetbrains.intellij.xml" % "xml-psi" % platformVersion intransitive(),
      "com.jetbrains.intellij.xml" % "xml-psi-impl" % platformVersion intransitive(),
      "com.jetbrains.intellij.xml" % "xml-parser" % platformVersion intransitive(),
      "com.jetbrains.intellij.xml" % "xml-syntax" % platformVersion intransitive(),
      // Libraries the plugin jars expect from the IDE distribution.
      "org.apache.maven" % "maven-artifact" % "3.9.9" intransitive(),
      // The version the IDE bundles (lib/intellij.libraries.markdown.jar, class for class): the plugin's ScalaDoc
      // Markdown parser extends its block constraints, which 0.7.17 changed (tag descriptions came out empty).
      "org.jetbrains" % "markdown-jvm" % "0.7.16" intransitive(),
      "org.apache.commons" % "commons-text" % "1.12.0" intransitive(),
      // LSP protocol types; the JSON-RPC layer (lsp4j.jsonrpc) comes with the plugin's jars, at this version.
      "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.20.1" intransitive(),
      "org.scalameta" %% "munit" % "1.3.4" % Test,
      // 2.13.18 is the plugin's latest 2.13 (ScalaVersion.Latest): its library-highlighting test data is for these sources.
      ("org.scala-lang" % "scala-library" % "2.13.18" % Fixtures intransitive()).withSources(),
      // The plugin's type-inference tests run on Scala 2.12 by default (ScalaSdkOwner.defaultSdkVersion).
      "org.scala-lang" % "scala-library" % "2.12.21" % Fixtures212 intransitive(),
    ),
    inlayPlatformJar := {
      val sources = update.value.select(configurationFilter(InlayPlatform.name), moduleFilter(name = "ide-impl"), artifactFilter(`type` = "jar"))
      val source = sources match {
        case Seq(jar) => jar
        case other => sys.error(s"expected one ide-impl jar for the hints slice, got: $other")
      }
      // Measured by replaying InlayHintsOracle with class-load logging against the full jar.
      val included = Seq(
        "com/intellij/codeInsight/hints/filtering/", // ScalaInlayParameterHintsPass: MethodInfoExcludeListFilter, the whole filtering package.
        "com/intellij/codeInsight/hint/TooltipGroup.class", // ImplicitHintsPass: ErrorTooltip initialization.
        "com/intellij/codeInsight/daemon/impl/HintRenderer.class", // TextPartsHintRenderer: expanded implicit labels in the oracle.
        "com/intellij/codeInsight/daemon/impl/HintRenderer$Companion.class", // TextPartsHintRenderer: its renderer base's companion.
        "com/intellij/openapi/editor/actions/EditorActionUtil.class", // SelectWordUtil: word/lexeme boundaries in the editor's lexer.
      )
      val directory = target.value / "inlay-platform"
      val out = directory / s"scala-inlay-platform-$platformVersion.jar"
      val slice = FileFunction.cached(streams.value.cacheDirectory / "inlay-platform", FilesInfo.hash, FilesInfo.exists) { _ =>
        val extracted = directory / "classes"
        IO.delete(extracted)
        val entries = IO.unzip(source, extracted, new NameFilter {
          def accept(name: String): Boolean = included.exists(entry => name == entry || entry.endsWith("/") && name.startsWith(entry))
        })
        IO.zip(entries.toSeq.filter(_.isFile).pair(Path.relativeTo(extracted)), out)
        Set(out)
      }
      slice(Set(source, baseDirectory.value / "build.sbt"))
      out
    },
    Compile / unmanagedJars ++= (scalaPluginJars.value :+ inlayPlatformJar.value).classpath,
    // One JVM per test run: the host installs process-wide platform singletons (Application, root area).
    Test / fork := true,
    dist := {
      val out = target.value / "dist"
      IO.delete(out)
      val pluginJars = scalaPluginJars.value.toSet
      // Jar names repeat across organizations (e.g. kernel-<version>.jar): managed jars are named with theirs.
      val dependencies = (Compile / dependencyClasspath).value.filter(_.data.getName.endsWith(".jar")).map { jar =>
        jar.data -> jar.get(moduleID.key).fold(jar.data.getName)(m => s"${m.organization}.${jar.data.getName}")
      }
      val jars = ((Compile / packageBin).value -> "scala-psi-host.jar") +: dependencies
      // The classpath in order (the host first), as paths relative to the distribution; the plugin's jars apart.
      val entries = jars.map { case (jar, name) =>
        val entry = (if (pluginJars(jar)) "lib/plugin/" else "lib/") + name
        if ((out / entry).exists) sys.error(s"two jars named ${jar.getName} on the classpath")
        IO.copyFile(jar, out / entry)
        entry
      }
      IO.writeLines(out / "lib" / "classpath", entries)
      for (legal <- Seq("LICENSE", "NOTICE")) IO.copyFile(baseDirectory.value / legal, out / legal)
      IO.copyFile(baseDirectory.value / "dist" / "scala-psi-host", out / "bin" / "scala-psi-host")
      (out / "bin" / "scala-psi-host").setExecutable(true)
      out
    },
    // The language server's fixture modules and the measured adapter ledger each need their own host JVM:
    // other suites may open the same modules or record calls outside the surface scenario.
    Test / testGrouping := {
      val isolated = Set("scalapsihost.lsp.LanguageServerTest", "scalapsihost.AdapterSurfaceTest")
      val (own, shared) = (Test / definedTests).value.partition(test => isolated(test.name))
      val fork = Tests.SubProcess((Test / forkOptions).value)
      Seq(Tests.Group("shared", shared, fork)) ++ own.map(test => Tests.Group(test.name, Seq(test), fork))
    },
    // Tools in the test scope (the type-inference oracle) get their own JVM with the same options.
    Test / run / fork := true,
    Test / run / javaOptions := (Test / javaOptions).value,
    Test / javaOptions ++= {
      val state = target.value / "host-state"
      val report = update.value
      def fixture(configuration: Configuration, classifier: String = "") =
        report.select(configurationFilter(configuration.name), moduleFilter(), artifactFilter(classifier = classifier)) match {
          case Seq(jar) => jar
          case other => sys.error(s"expected exactly one ${if (classifier.isEmpty) "jar" else classifier + " jar"} in ${configuration.name}, got: $other")
        }
      Seq(
        // Indexed library PSI is AST-backed (the IDE keeps it stub-backed): the whole scala-library needs > 768 MB.
        "-Xmx2g",
        "-Djava.awt.headless=true",
        // As the IDE's and the plugin's test JVMs: in unit-test mode the platform records annotation-builder creation
        // traces through Throwable internals.
        "--add-opens=java.base/java.lang=ALL-UNNAMED",
        s"-Dscalapsihost.stateDir=$state",
        s"-Dscalapsihost.scalaPluginJars=${scalaPluginJars.value.mkString(java.io.File.pathSeparator)}",
        s"-Dscalapsihost.fixture.scalaLibrary=${fixture(Fixtures)}",
        s"-Dscalapsihost.fixture.scalaLibrarySources=${fixture(Fixtures, "sources")}",
        s"-Dscalapsihost.fixture.scalaLibrary212=${fixture(Fixtures212)}",
      )
    },
  )
