// The Scala plugin and platform 263.x are class file 69: compiling against them (javac) and running them needs JDK 25.
val requiredJavaVersion = 25

val platformVersion = "263.7307.44"

// The Scala plugin's own jars, built from intellij-scala with `sbt packageArtifactZip`.
val scalaPluginLib = settingKey[File]("Directory with the Scala plugin's jars (Scala/lib)")

// Library binaries the tests index. Kept out of the compile/test classpath on purpose.
val Fixtures = config("fixtures").hide
// A second configuration: two versions of one module in a single configuration would evict each other.
val Fixtures212 = config("fixtures212").hide

val scalaPluginJars = taskKey[Seq[File]]("The Scala plugin's jars that go on the classpath")

ThisBuild / scalaVersion := "3.9.0"

lazy val root = (project in file("."))
  .configs(Fixtures, Fixtures212)
  .settings(
    name := "scala-psi-host",
    initialize := {
      val _ = initialize.value
      val running = java.lang.Runtime.version()
      if (running.feature() < requiredJavaVersion)
        sys.error(s"scala-psi-host needs sbt to run on JDK $requiredJavaVersion+ (found $running); use `sbt --java-home <jdk25>`")
    },
    scalaPluginLib := file(sys.env.getOrElse("SCALA_PLUGIN_LIB", "../intellij-scala/target/plugin/Scala/lib")).getCanonicalFile,
    scalaPluginJars := {
      val lib = scalaPluginLib.value
      if (!lib.isDirectory) sys.error(s"Scala plugin jars not found at $lib (set SCALA_PLUGIN_LIB)")
      // The plugin bundles its own Scala runtime; this build provides one.
      (lib * "*.jar").get.filterNot(f => Set("scala-library.jar", "scala3-library_3.jar")(f.getName))
    },
    resolvers ++= Seq(
      "intellij-snapshots" at "https://www.jetbrains.com/intellij-repository/snapshots",
      "intellij-dependencies" at "https://cache-redirector.jetbrains.com/intellij-dependencies",
    ),
    libraryDependencies ++= Seq(
      "com.jetbrains.intellij.platform" % "core-impl" % platformVersion,
      "com.jetbrains.intellij.platform" % "analysis-impl" % platformVersion,
      "com.jetbrains.intellij.java" % "java-psi-impl" % platformVersion,
      "com.jetbrains.intellij.platform" % "indexing-impl" % platformVersion,
      // IndentHelperImpl: PSI copies record their indentation (ChangeUtil.copyElement). Only this jar, not the formatter.
      "com.jetbrains.intellij.platform" % "code-style-impl" % platformVersion intransitive(),
      // Java stub factories and serializers (JavaStubRegistryExtension): the stub schema of Java class files.
      "com.jetbrains.intellij.java" % "java-indexing-serializers" % platformVersion,
      // XML PSI: ScalaDoc and XML literals parse through it.
      "com.jetbrains.intellij.xml" % "xml-psi" % platformVersion intransitive(),
      "com.jetbrains.intellij.xml" % "xml-psi-impl" % platformVersion intransitive(),
      "com.jetbrains.intellij.xml" % "xml-parser" % platformVersion intransitive(),
      "com.jetbrains.intellij.xml" % "xml-syntax" % platformVersion intransitive(),
      // Libraries the plugin jars expect from the IDE distribution.
      "org.apache.maven" % "maven-artifact" % "3.9.9" intransitive(),
      "org.jetbrains" % "markdown-jvm" % "0.7.3" intransitive(),
      "org.apache.commons" % "commons-text" % "1.12.0" intransitive(),
      "org.scalameta" %% "munit" % "1.3.4" % Test,
      "org.scala-lang" % "scala-library" % "2.13.16" % Fixtures intransitive(),
      // The plugin's type-inference tests run on Scala 2.12 by default (ScalaSdkOwner.defaultSdkVersion).
      "org.scala-lang" % "scala-library" % "2.12.21" % Fixtures212 intransitive(),
    ),
    Compile / unmanagedJars ++= scalaPluginJars.value.classpath,
    // One JVM per test run: the host installs process-wide platform singletons (Application, root area).
    Test / fork := true,
    // Tools in the test scope (the type-inference oracle) get their own JVM with the same options.
    Test / run / fork := true,
    Test / run / javaOptions := (Test / javaOptions).value,
    Test / javaOptions ++= {
      val state = target.value / "host-state"
      val report = update.value
      def fixture(configuration: Configuration) = report.select(configurationFilter(configuration.name)) match {
        case Seq(jar) => jar
        case other => sys.error(s"expected exactly one jar in ${configuration.name}, got: $other")
      }
      Seq(
        // Indexed library PSI is AST-backed (the IDE keeps it stub-backed): the whole scala-library needs > 768 MB.
        "-Xmx2g",
        "-Djava.awt.headless=true",
        s"-Dscalapsihost.stateDir=$state",
        s"-Dscalapsihost.scalaPluginJars=${scalaPluginJars.value.mkString(java.io.File.pathSeparator)}",
        s"-Dscalapsihost.fixture.scalaLibrary=${fixture(Fixtures)}",
        s"-Dscalapsihost.fixture.scalaLibrary212=${fixture(Fixtures212)}",
      )
    },
  )
