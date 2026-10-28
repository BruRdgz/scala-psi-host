package scalapsihost

import com.intellij.lang.annotation.HighlightSeverity

import java.nio.file.Path

/**
 * Modules as a build server describes them: each resolves against its own classpath, compiler options and module
 * dependencies. Expectations are what scalac reports when compiling the same code in the same setup.
 */
class ModuleTest extends munit.FunSuite {
  private def host = TestHost.host
  private val library213 = Path.of(sys.props("scalapsihost.fixture.scalaLibrary"))
  private val library212 = Path.of(sys.props("scalapsihost.fixture.scalaLibrary212"))

  private lazy val (scala213, scala212, noPredef) = {
    TestHost.libraryReport // the JDK
    (
      host.addModule(ModuleSpec("scala213", "2.13.18", Seq.empty, Seq(library213))),
      host.addModule(ModuleSpec("scala212", "2.12.21", Seq.empty, Seq(library212))),
      host.addModule(ModuleSpec("noPredef", "2.13.18", Seq("-Yno-predef"), Seq(library213))),
    )
  }

  private def errorsIn(module: HostModule, source: String): Seq[String] = {
    val file = host.openSource(source, module)
    try host.diagnostics(file).filter(_.severity == HighlightSeverity.ERROR).map(_.message)
    finally host.closeSource(file)
  }

  test("each module resolves against its own classpath") {
    val source = "object UsesLazyList { val l = LazyList(1, 2) }"
    assertEquals(errorsIn(scala213, source), Seq.empty)
    val in212 = errorsIn(scala212, source)
    assert(in212.contains("Cannot resolve symbol LazyList"), in212)
  }

  test("compiler options from the build apply to the module's files") {
    val source = """object UsesPredef { def f(): Unit = println("x") }"""
    assertEquals(errorsIn(scala213, source), Seq.empty)
    assertEquals(errorsIn(noPredef, source), Seq("Cannot resolve symbol println"))
  }

  test("a module sees the sources of the modules it depends on, and no others") {
    val lib = host.addModule(ModuleSpec("lib", "2.13.18", Seq.empty, Seq(library213)))
    val client = host.addModule(ModuleSpec("client", "2.13.18", Seq.empty, Seq(library213), dependsOn = Seq("lib")))
    val libFile = host.openSource("package lib\nobject Lib { val answer: Int = 42 }", lib)
    try {
      val clientSource = "object Client { val n: Int = lib.Lib.answer }"
      assertEquals(errorsIn(client, clientSource), Seq.empty)
      assertEquals(errorsIn(scala213, clientSource), Seq("Cannot resolve symbol lib"))
    } finally host.closeSource(libFile)
  }
}
