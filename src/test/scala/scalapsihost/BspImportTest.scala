package scalapsihost

import com.intellij.lang.annotation.HighlightSeverity

import java.net.UnixDomainSocketAddress
import java.nio.channels.ServerSocketChannel
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.util.Using

/**
 * A real build through BSP: `fixtures/bsp-project` (sbt; `app` depends on `core` and compiles with `-Xsource:3`;
 * `core` mixes Scala and Java).
 * Needs the fixture's connection file: run `sbt bspConfig` in the fixture once. scalac compiles the fixture without
 * errors, so the host must report none.
 */
class BspImportTest extends munit.FunSuite {
  override val munitTimeout: Duration = 10.minutes

  private val fixture = Path.of("fixtures/bsp-project").toAbsolutePath
  private lazy val imported = BspImport.importBuild(fixture)
  private def module(name: String) = imported.find(_.spec.name == name).getOrElse(fail(s"no module $name in ${imported.map(_.spec.name)}"))

  test("sbt server liveness distinguishes missing, stale and uncertain metadata") {
    import BspImport.SbtServerState.*
    val root = Files.createTempDirectory(Path.of("target"), "sbt-")
    val active = Files.createDirectories(root.resolve("project/target")).resolve("active.json")
    val socket = root.toAbsolutePath.resolve("sock")
    try {
      assertEquals(BspImport.sbtServerState(root), Missing)
      for (text <- Seq("{", "{}", """{"uri":"tcp://127.0.0.1:1234"}""")) {
        Files.writeString(active, text)
        assert(BspImport.sbtServerState(root) match {
          case Unknown(_) => true
          case _ => false
        }, text)
      }
      Files.writeString(active, s"""{"uri":"local://$socket"}""")
      Using.resource(ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX)) { listener =>
        listener.bind(UnixDomainSocketAddress.of(socket))
        assertEquals(BspImport.sbtServerState(root), Live(socket))
      }
      assert(Files.exists(socket), "a leftover socket must not establish liveness")
      assertEquals(BspImport.sbtServerState(root), Inactive(socket))
      Files.delete(socket)
      assertEquals(BspImport.sbtServerState(root), Inactive(socket))
    } finally {
      Files.deleteIfExists(socket)
      Files.deleteIfExists(active)
      Files.delete(root.resolve("project/target"))
      Files.delete(root.resolve("project"))
      Files.delete(root)
    }
  }

  test("only the connection that started sbt stops it, including after a reload") {
    import BspImport.SbtServerState.*
    assert(BspImport.sbtServerState(fixture) match {
      case Missing | Inactive(_) => true
      case _ => false
    }, "the ownership test needs an idle fixture; an existing external server is left untouched")
    val owner = BspImport.connect(fixture)
    try {
      val live = BspImport.sbtServerState(fixture)
      assert(live match {
        case Live(_) => true
        case _ => false
      }, live.toString)
      Using.resource(BspImport.connect(fixture)) { borrowed =>
        assert(borrowed.modules().nonEmpty)
      }
      assertEquals(BspImport.sbtServerState(fixture), live, "closing a borrowed connection stopped sbt")
      owner.reload()
      assertEquals(BspImport.sbtServerState(fixture), live, "reimport restarted sbt")
    } finally owner.close()
    assert(!Files.exists(fixture.resolve("project/target/active.json")), "the owned sbt server survived close")
    owner.close()
    assert(!Files.exists(fixture.resolve("project/target/active.json")), "a repeated close started sbt again")
  }

  test("imports the build's Scala targets with their settings") {
    val app = module("app")
    assertEquals(app.spec.scalaVersion, "2.13.18")
    assertEquals(app.spec.dependsOn, Seq("core"))
    assert(app.spec.scalacOptions.contains("-Xsource:3"), app.spec.scalacOptions)
    assert(app.spec.classpath.exists(_.getFileName.toString == "scala-library-2.13.18.jar"), app.spec.classpath)
    assertEquals(app.sources.map(fixture.relativize(_).toString), Seq("app/src/main/scala/app/Main.scala"))
    assert(!module("core").spec.scalacOptions.contains("-Xsource:3"))
    assert(module("core").sources.map(fixture.relativize(_).toString).contains("core/src/main/java/core/Shout.java"))
  }

  // In the fixture itself, with a broken file for the duration of the test, so its build outputs remain reusable.
  test("compiling through the build server reports the compiler's errors by file and position") {
    val broken = fixture.resolve("core/src/main/scala/core/Broken.scala")
    Files.writeString(broken, "package core\n\nobject Broken {\n  val n: Int = \"one\"\n}\n")
    val result =
      try Using.resource(BspImport.connect(fixture))(_.compile())
      finally Files.delete(broken)
    assert(!result.succeeded)
    val errors = result.diagnostics.getOrElse(broken, Seq.empty).filter(_.severity == HighlightSeverity.ERROR)
    assertEquals(errors.map(_.start.line), Seq(3))
    assert(errors.head.message.contains("type mismatch"), errors)
  }

  test("compiles a document of a Scala 3 module with the module's compiler") {
    val core3 = module("core3")
    // The module's class output holds its other files' classes (Greet3), which the document uses.
    Using.resource(BspImport.connect(fixture))(_.compile())
    val compiler = new DocumentCompiler
    def errors(text: String) = compiler.compile(core3.compile, "Use.scala", text).filter(_.severity == HighlightSeverity.ERROR)

    assertEquals(errors("package core3\n\nobject Use:\n  val s: String = Greet3.hello(\"x\")\n"), Seq.empty)
    // The error alone: not the compiler's summary of the run ("1 error found").
    val mismatch = compiler.compile(core3.compile, "Use.scala", "package core3\n\nobject Use:\n  val n: Int = Greet3.hello(\"x\")\n")
    assertEquals(mismatch.map(d => (d.severity, d.start.line)), Seq((HighlightSeverity.ERROR, 3)))
    assert(mismatch.head.message.contains("Required: Int"), mismatch)
  }

  test("the imported project highlights without errors") {
    TestHost.libraryReport // the JDK
    val hostModules = imported.map(m => m -> TestHost.host.addModule(m.spec))
    val files = for ((m, hostModule) <- hostModules; source <- m.sources) yield {
      fixture.relativize(source).toString -> TestHost.host.openFile(source, Files.readString(source), hostModule)
    }
    try {
      val errors = files.collect { case (path, file: org.jetbrains.plugins.scala.lang.psi.api.ScalaFile) => path -> file }.flatMap { (path, file) =>
        TestHost.host.diagnostics(file).filter(_.severity == HighlightSeverity.ERROR).map(d => s"$path ${d.range}: ${d.message}")
      }
      assertEquals(errors, Seq.empty)
      // Java source PSI has its typed leaves (the class name is a PsiIdentifier).
      val javaClasses = TestHost.host.read {
        files.collect { case (_, file: com.intellij.psi.PsiJavaFile) => file.getClasses.toSeq.map(_.getName) }.flatten
      }
      assertEquals(javaClasses, Seq("Shout"))
      // Nothing left the host's tables or was logged as an error on the way (both would hide problems).
      val swallowed = CallLog.snapshot.map(_._1).filter(e => e.startsWith("UNSUPPORTED ") || e.startsWith("LOGGED ERROR "))
      assertEquals(swallowed, Seq.empty)
    } finally files.foreach((_, file) => TestHost.host.closeSource(file))
  }
}
