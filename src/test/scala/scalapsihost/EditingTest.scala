package scalapsihost

import com.intellij.lang.annotation.HighlightSeverity
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

import java.nio.file.Path

/**
 * Editing as an editor does it: a change to one file's text is reparsed in place and reaches every query that
 * depends on it, including other files' highlighting. Expectations are what scalac reports for each version.
 */
class EditingTest extends munit.FunSuite {
  private def host = TestHost.host
  private lazy val module = {
    TestHost.libraryReport // the JDK
    host.addModule(ModuleSpec("editing", "2.13.18", Seq.empty, Seq(Path.of(sys.props("scalapsihost.fixture.scalaLibrary")))))
  }

  private def errors(file: ScalaFile): Seq[String] =
    host.diagnostics(file).filter(_.severity == HighlightSeverity.ERROR).map(_.message)

  // The platform picks a physical file's language; for a module on Scala 3 the plugin's substitutor makes it Scala 3.
  test("a source of a Scala 3 module is Scala 3") {
    val scala3 = host.addModule(ModuleSpec("editing3", "3.3.4", Seq.empty, Seq(Path.of(sys.props("scalapsihost.fixture.scalaLibrary")))))
    val file = host.openFile(Path.of("/editing3/C.scala"), "object C:\n  def f(b: Boolean): Int = if b then 1 else 2\n", scala3)
    try {
      assertEquals(file.getLanguage, org.jetbrains.plugins.scala.Scala3Language.INSTANCE)
      assertEquals(errors(file.asInstanceOf[ScalaFile]), Seq.empty)
    } finally host.closeSource(file)
  }

  test("an edit is reparsed in place and changes what other files see") {
    val aText = "object A { def f: Int = 1 }\n"
    val a = host.openFile(Path.of("/editing/A.scala"), aText, module).asInstanceOf[ScalaFile]
    val b = host.openFile(Path.of("/editing/B.scala"), "object B { val x: Int = A.f }\n", module).asInstanceOf[ScalaFile]
    try {
      assertEquals(errors(b), Seq.empty)

      val start = aText.indexOf("Int = 1")
      host.edit(a, start, start + "Int = 1".length, "String = \"s\"")

      assertEquals(a.getText, "object A { def f: String = \"s\" }\n")
      assert(a.isValid, "the edited file is the same PSI file, reparsed in place")
      assertEquals(errors(b), Seq("Type mismatch. Required: Int, found: String"))
      // Nothing left the host's tables or was logged as an error on the way (both would hide problems).
      val swallowed = CallLog.snapshot.map(_._1).filter(e => e.startsWith("UNSUPPORTED ") || e.startsWith("LOGGED ERROR "))
      assertEquals(swallowed, Seq.empty)
    } finally {
      host.closeSource(a)
      host.closeSource(b)
    }
  }
}
