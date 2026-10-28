package scalapsihost

import com.intellij.lang.annotation.HighlightSeverity

import java.nio.file.{Files, Path}
import scala.concurrent.duration.*

/**
 * A real build through BSP: `fixtures/bsp-project` (sbt; `app` depends on `core` and compiles with `-Xsource:3`).
 * Needs the fixture's connection file: run `sbt bspConfig` in the fixture once. scalac compiles the fixture without
 * errors, so the host must report none.
 */
class BspImportTest extends munit.FunSuite {
  override val munitTimeout: Duration = 10.minutes

  private val fixture = Path.of("fixtures/bsp-project").toAbsolutePath
  private lazy val imported = BspImport.importBuild(fixture)
  private def module(name: String) = imported.find(_.spec.name == name).getOrElse(fail(s"no module $name in ${imported.map(_.spec.name)}"))

  test("imports the build's Scala targets with their settings") {
    val app = module("app")
    assertEquals(app.spec.scalaVersion, "2.13.18")
    assertEquals(app.spec.dependsOn, Seq("core"))
    assert(app.spec.scalacOptions.contains("-Xsource:3"), app.spec.scalacOptions)
    assert(app.spec.classpath.exists(_.getFileName.toString == "scala-library-2.13.18.jar"), app.spec.classpath)
    assertEquals(app.sources.map(fixture.relativize(_).toString), Seq("app/src/main/scala/app/Main.scala"))
    assert(!module("core").spec.scalacOptions.contains("-Xsource:3"))
  }

  test("the imported project highlights without errors") {
    TestHost.libraryReport // the JDK
    // Dependencies before dependents, as the build lists them.
    val ordered = Iterator.iterate((Seq.empty[ImportedModule], imported)) { (done, rest) =>
      val (ready, waiting) = rest.partition(_.spec.dependsOn.forall(d => done.exists(_.spec.name == d)))
      (done ++ ready, waiting)
    }.dropWhile(_._2.nonEmpty).next()._1
    val hostModules = ordered.map(m => m -> TestHost.host.addModule(m.spec))
    val files = for ((m, hostModule) <- hostModules; source <- m.sources)
      yield fixture.relativize(source).toString -> TestHost.host.openSource(Files.readString(source), hostModule)
    try {
      val errors = files.flatMap { (path, file) =>
        TestHost.host.diagnostics(file).filter(_.severity == HighlightSeverity.ERROR).map(d => s"$path ${d.range}: ${d.message}")
      }
      assertEquals(errors, Seq.empty)
    } finally files.foreach((_, file) => TestHost.host.closeSource(file))
  }
}
