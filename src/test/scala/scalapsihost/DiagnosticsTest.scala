package scalapsihost

import com.intellij.lang.annotation.HighlightSeverity

/**
 * The plugin's highlighting through the host. Expected problems are the ones scalac reports for the same code, with
 * the plugin's messages (ScalaBundle).
 */
class DiagnosticsTest extends munit.FunSuite {
  private def problems(file: org.jetbrains.plugins.scala.lang.psi.api.ScalaFile): Seq[(String, String)] =
    TestHost.host.diagnostics(file)
      .filter(_.severity.compareTo(HighlightSeverity.WARNING) >= 0)
      .map(d => d.message -> d.range.substring(file.getText))

  test("a correct file has no problems") {
    assertEquals(problems(TestHost.scenarioFile()), Seq.empty)
  }

  test("reports type mismatches, unresolved names and missing arguments") {
    TestHost.libraryReport
    val file = TestHost.host.openSource(
      """object Errors {
        |  def f(a: Int): Int = a
        |  val x: Int = "s"
        |  val y = undefinedName
        |  val z = f()
        |}
        |""".stripMargin)
    try {
      val found = problems(file)
      assert(found.contains("Type mismatch. Required: Int, found: String" -> "\"s\""), found)
      assert(found.contains("Cannot resolve symbol undefinedName" -> "undefinedName"), found)
      assert(found.exists(_._1 == "Unspecified value parameters: a: Int"), found)
      assertEquals(found.size, 3, found)
    } finally TestHost.host.closeSource(file)
  }

  test("reports syntax errors") {
    val file = TestHost.host.parse("object Broken { val = 1 }")
    assert(TestHost.host.diagnostics(file).exists(_.severity == HighlightSeverity.ERROR))
  }
}
