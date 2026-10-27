package scalapsihost

import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScPatternDefinition
import com.intellij.psi.util.PsiTreeUtil

import java.io.File
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** The one host of the test JVM (see `Test / fork` in build.sbt), with the scala-library fixture indexed. */
object TestHost {
  private def property(name: String): String =
    sys.props.getOrElse(name, throw new IllegalStateException(s"system property $name is not set; run the tests through sbt"))

  lazy val host: ScalaPsiHost = ScalaPsiHost.start(HostConfig(
    stateDir = Path.of(property("scalapsihost.stateDir")),
    scalaPluginJars = property("scalapsihost.scalaPluginJars").split(File.pathSeparator).toSeq.map(Path.of(_)),
  ))

  /** Only the root `scala` package for now: the full library and the JDK are phase F1. */
  lazy val libraryReport: LibraryIndexReport =
    host.indexLibrary(Path.of(property("scalapsihost.fixture.scalaLibrary")), _.matches("scala/[A-Za-z0-9]+\\.class"))

  val scenarioSource: String =
    """object Main {
      |  val a = 1
      |  val b = a
      |  def inc(x: Int): Int = x + 1
      |  val c = inc(b)
      |  class Box[A](val v: A) {
      |    def map[B](f: A => B): Box[B] = new Box(f(v))
      |  }
      |  val d = for (x <- new Box(c)) yield x
      |}
      |""".stripMargin

  /** Parses the scenario with the library indexed, so every query sees the same symbols. */
  def scenarioFile(): ScalaFile = {
    libraryReport
    host.parse(scenarioSource)
  }

  def references(file: ScalaFile): Seq[ScReferenceExpression] =
    PsiTreeUtil.findChildrenOfType(file, classOf[ScReferenceExpression]).asScala.toSeq

  def patternDefinitions(file: ScalaFile): Seq[ScPatternDefinition] =
    PsiTreeUtil.findChildrenOfType(file, classOf[ScPatternDefinition]).asScala.toSeq

  /** Runs every query the semantic tests make; the adapter surface test measures this. */
  def runScenario(): Unit = {
    val file = scenarioFile()
    references(file).foreach(_.resolve())
    patternDefinitions(file).flatMap(_.expr).foreach(_.`type`())
  }
}
