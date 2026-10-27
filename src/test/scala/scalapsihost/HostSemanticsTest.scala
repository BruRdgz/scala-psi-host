package scalapsihost

import com.intellij.psi.PsiErrorElement
import com.intellij.psi.util.PsiTreeUtil

/**
 * The plugin's parser, resolver and type checker answering through the host. Expectations are what the Scala
 * compiler (and the plugin inside IntelliJ) give for the scenario, not what the host currently returns.
 */
class HostSemanticsTest extends munit.FunSuite {
  private lazy val file = TestHost.scenarioFile()

  test("parses into Scala PSI without syntax errors") {
    assertEquals(PsiTreeUtil.findChildrenOfType(file, classOf[PsiErrorElement]).size(), 0)
  }

  // Each reference in source order, and the text of the declaration it must resolve to in the same file.
  private val localResolves = Seq(
    "a" -> "a",
    "x" -> "x: Int",
    "inc" -> "def inc(x: Int): Int = x + 1",
    "b" -> "b",
    "f" -> "f: A => B",
    "v" -> "val v: A",
    "c" -> "c",
    "x" -> "x",
  )

  test("resolves references to their local declarations") {
    val resolved = TestHost.references(file).filterNot(_.getText == "+").map { ref =>
      val target = ref.resolve()
      assert(target != null, s"'${ref.getText}' is unresolved")
      assertEquals(target.getContainingFile, file, s"'${ref.getText}' resolved outside the file")
      ref.getText -> target.getText
    }
    assertEquals(resolved, localResolves)
  }

  test("resolves Int's + (needs the full library and JDK index, phase F1)".fail) {
    val plus = TestHost.references(file).find(_.getText == "+").get
    assert(plus.resolve() != null, "'+' is unresolved")
  }

  private def typeOf(definition: String): String = {
    val pattern = TestHost.patternDefinitions(file).find(_.getText.startsWith(definition)).get
    pattern.expr.get.`type`().fold(failure => s"<failure: $failure>", _.presentableText(using pattern))
  }

  test("types literals and local references") {
    assertEquals(typeOf("val a"), "Int")
    assertEquals(typeOf("val b"), "Int")
  }

  // Wrong without the full library and JDK: the type checker degrades instead of failing.
  test("types a method application (phase F1: currently Any)".fail) {
    assertEquals(typeOf("val c"), "Int")
  }

  test("types a for-comprehension through map (phase F1: currently Box[Nothing])".fail) {
    assertEquals(typeOf("val d"), "Box[Int]")
  }
}
