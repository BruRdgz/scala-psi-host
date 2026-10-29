package scalapsihost

import java.nio.file.{Files, Path}
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * Bootstrap guard: the set of adapter entries the scenario reaches must equal the reviewed surface in
 * `adapter-surface.txt`. A new platform or plugin version that asks the host for something else fails here,
 * with the difference, instead of changing behaviour unnoticed.
 */
class AdapterSurfaceTest extends munit.FunSuite {

  // Index and classpath lookups vary with the symbols indexed (phase F1), not with the adapter surface.
  private def isSymbolLookup(entry: String) = entry.startsWith("GAP ") || entry.startsWith("HIT ")

  test("the scenario stays within the host's tables") {
    TestHost.runScenario()
    val unsupported = CallLog.snapshot.map(_._1).filter(_.startsWith("UNSUPPORTED "))
    assertEquals(unsupported, Seq.empty)
  }

  test("completion beans retain their descriptor implementations and weigher IDs") {
    TestHost.host.read {
      val contributors = com.intellij.codeInsight.completion.CompletionContributor.forLanguage(
        org.jetbrains.plugins.scala.ScalaLanguage.INSTANCE, null).asScala.map(_.getClass.getName).toSet
      assertEquals(contributors, ScalaPsiHost.CompletionContributors)
      val weighers = com.intellij.psi.WeighingService.getWeighers(com.intellij.codeInsight.completion.CompletionService.RELEVANCE_KEY)
        .asScala.map(_.toString).toSeq
      assertEquals(weighers.toSet, Set("scalaContainingClassWeigher", "scalaMethodCompletionWeigher", "scalaByNameWeigher",
        "scalaKindWeigher", "ScopeWeigher", "stats", "priority", "prefix", "grouping", "explicitProximity", "proximity"))
      assert(weighers.indexOf("scalaContainingClassWeigher") < weighers.indexOf("scalaKindWeigher"), weighers)
      val proximity = com.intellij.psi.WeighingService.getWeighers(com.intellij.psi.util.proximity.PsiProximityComparator.WEIGHER_KEY)
        .asScala.map(_.toString).toSet
      assertEquals(proximity, Set("scalaClassObjectWeigher", "scalaExplicitlyImportedWeigher"))
    }
  }

  test("the adapter surface matches the reviewed list") {
    TestHost.runScenario()
    val actual = CallLog.snapshot.map(_._1).filterNot(isSymbolLookup)
    val expected = Using.resource(Source.fromResource("adapter-surface.txt"))(_.getLines().filter(_.nonEmpty).toSeq)
    if (actual != expected) {
      val out = Path.of(sys.props("scalapsihost.stateDir")).resolve("adapter-surface.actual.txt")
      Files.write(out, actual.asJava)
      fail(
        s"""adapter surface changed (actual list written to $out)
           |added:   ${actual.diff(expected).mkString("\n         ")}
           |removed: ${expected.diff(actual).mkString("\n         ")}""".stripMargin)
    }
  }
}
