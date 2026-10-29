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

  /** scala-library 2.13 and the test JVM's JDK, the classpath of a plain Scala 2.13 project. */
  lazy val libraryReport: LibraryIndexReport = {
    host.addJdk(Path.of(System.getProperty("java.home")))
    host.addLibrary(Path.of(property("scalapsihost.fixture.scalaLibrary")))
  }

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

  /** The scenario as the project's one source file, opened after the libraries so every query sees the same symbols. */
  private lazy val scenario: ScalaFile = {
    libraryReport
    host.openSource(scenarioSource)
  }

  private lazy val completionModule = host.addModule(ModuleSpec("surface-completion", "2.13.18", Seq.empty,
    Seq(Path.of(property("scalapsihost.fixture.scalaLibrary")))))

  def scenarioFile(): ScalaFile = scenario

  /** Applies replacements at original-text offsets, checking that no two edits overlap. */
  def applyEdits(text: String, edits: Seq[ChangeEngine.Edit]): String = {
    val sorted = edits.sortBy(_.range.getStartOffset)
    sorted.sliding(2).foreach {
      case Seq(left, right) =>
        require(left.range.getStartOffset < right.range.getStartOffset &&
          left.range.getEndOffset <= right.range.getStartOffset, "edits overlap")
      case _ => ()
    }
    sorted.reverse.foldLeft(text) { (current, edit) =>
      current.substring(0, edit.range.getStartOffset) + edit.text + current.substring(edit.range.getEndOffset)
    }
  }

  def references(file: ScalaFile): Seq[ScReferenceExpression] =
    PsiTreeUtil.findChildrenOfType(file, classOf[ScReferenceExpression]).asScala.toSeq

  def patternDefinitions(file: ScalaFile): Seq[ScPatternDefinition] =
    PsiTreeUtil.findChildrenOfType(file, classOf[ScPatternDefinition]).asScala.toSeq

  /** Runs every query the semantic tests make; the adapter surface test measures this. */
  def runScenario(): Unit = {
    val file = scenarioFile()
    host.read {
      references(file).foreach(_.resolve())
      patternDefinitions(file).flatMap(_.expr).foreach(_.`type`())
      val shortNames = com.intellij.psi.search.PsiShortNamesCache.getInstance(host.project)
      shortNames.getAllClassNames
      shortNames.getClassesByName("Box", com.intellij.psi.search.GlobalSearchScope.allScope(host.project))
    }
    host.completion(file, scenarioSource.indexOf("inc(b)") + "inc".length)
    host.completion(file, scenarioSource.indexOf("x + 1") + "x +".length)
    val completionSource = "object InsertScenario { val x: ArrayBuff = ???; def invoke(n: Int): Unit = (); inv }"
    val completionFile = host.openFile(Path.of("/surface/Insert.scala"), completionSource, completionModule) match {
      case f: ScalaFile => f
      case other => throw new IllegalStateException(s"not a Scala scenario: $other")
    }
    try {
      host.completion(completionFile, completionSource.indexOf("ArrayBuff") + "ArrayBuff".length)
      host.completion(completionFile, completionSource.indexOf("; inv") + "; inv".length)
      host.edit(completionFile, 0, completionSource.length, completionSource.replace("ArrayBuff", "ArrayBuffer"))
      host.edit(completionFile, 0, host.read(completionFile.getTextLength), completionSource)
      host.diagnostics(completionFile)
    } finally host.closeSource(completionFile)
    for ((text, prefix) <- Seq(
      ("object StringScenario { val member = 1; val s = \"$mem\" }", "$mem"),
      ("object KeywordScenario {\n  val n = 1\n  n mat\n}", "mat"),
      ("trait ImplementScenario { def run(): Unit }; object Use { new ImplementScen }", "ImplementScen"),
      ("object ConstructorScenario { val value = new ArrayBuff }", "ArrayBuff"))) {
      val source = host.openSource(text)
      try host.completion(source, text.lastIndexOf(prefix) + prefix.length)
      finally host.closeSource(source)
    }
    val settingsManager = com.intellij.psi.codeStyle.CodeStyleSettingsManager.getInstance(host.project)
    val settings = host.read(settingsManager.getCurrentSettings.clone())
    settings.getCustomSettings(classOf[org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings])
      .setImportsWithPrefix(Array("scala.collection.mutable._"))
    val prefixedText = "object PrefixScenario { val value = new ArrayBuff }"
    val prefixed = host.openFile(Path.of("/surface/Prefix.scala"), prefixedText, completionModule) match {
      case f: ScalaFile => f
      case other => throw new IllegalStateException(s"not a Scala scenario: $other")
    }
    try settingsManager.computeWithLocalSettings(settings, _ =>
      host.completion(prefixed, prefixedText.indexOf("ArrayBuff") + "ArrayBuff".length))
    finally host.closeSource(prefixed)
    host.formatting(file, None, 2, useTabs = false)
    val importFile = host.parse("object ImportUser { val x: scala.collection.mutable.HashSet[Int] = ??? }")
    val target = host.read {
      com.intellij.psi.JavaPsiFacade.getInstance(host.project).findClass("scala.collection.mutable.HashSet",
        com.intellij.psi.search.GlobalSearchScope.allScope(host.project))
    }
    ChangeEngine.run(importFile)(_.addImportForClass(target, null))

  }
}
