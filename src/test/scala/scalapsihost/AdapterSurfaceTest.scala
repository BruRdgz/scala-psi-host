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
