package scalapsihost.oracle

import com.intellij.openapi.util.TextRange
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.surroundWith.descriptors.ScalaExpressionSurroundDescriptor
import scalapsihost.{HostLog, ModuleSpec, TestHost}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/** Replays SurroundWithTest's file fixtures through the host's code actions, in Scala 2 and 3. */
object CodeActionsOracle {
  private val EndMarkers = Seq("<if>", "<else>", "<while>", "<do>", "<for>", "<yield>", "<catch>",
    "<finally>", "<try>", "<braces>", "<match>", "<parenthesis>", "<if_cond>", "<else_cond>", "<unary>")

  def main(args: Array[String]): Unit = {
    val Seq(repo, report) = args.toSeq
    val host = TestHost.host
    TestHost.libraryReport
    val library = Path.of(sys.props("scalapsihost.fixture.scalaLibrary"))
    val modules = Seq("2.13.18", "3.9.0").map(version =>
      host.addModule(ModuleSpec(s"actions-$version", version, Seq.empty, Seq(library))))
    val root = Path.of(repo).resolve("scala/scala-impl/testdata/surroundWith/data")
    val paths = Using.resource(Files.walk(root))(_.iterator.asScala.filter(_.toString.endsWith(".test")).toVector.sorted)
    val results = paths.map { path =>
      val name = root.relativize(path).toString
      val sections = Files.readString(path).replace("\r\n", "\n").split("\\n?(?m)^-{4,}\\n?", -1)
      val before = sections.head.trim
      val index = EndMarkers.indexWhere(before.contains)
      val start = before.indexOf("<start>")
      val errors = HostLog.count(HostLog.Level.Error)
      val result = if (sections.length != 2 || index < 0 || start < 0) "skip: unsupported fixture layout"
      else {
        val marker = EndMarkers(index)
        val end = before.indexOf(marker) - "<start>".length
        val original = before.replace("<start>", "").replace(marker, "")
        val expected = sections(1).replace("<preserve-trailing-spaces>", "").trim
        val scala3 = name.startsWith("3/")
        val file = host.openFile(Path.of(s"/actions-oracle/$name.scala"), original, modules(if (scala3) 1 else 0))
          .asInstanceOf[ScalaFile]
        try {
          val manager = com.intellij.psi.codeStyle.CodeStyleSettingsManager.getInstance(host.project)
          val settings = host.read(manager.getCurrentSettings.clone())
          settings.getCustomSettings(classOf[ScalaCodeStyleSettings]).USE_SCALA3_INDENTATION_BASED_SYNTAX = scala3
          manager.computeWithLocalSettings(settings, _ => {
            val title = new ScalaExpressionSurroundDescriptor().getSurrounders.apply(index).getTemplateDescription
            val actions = host.codeActions(file, new TextRange(start, end))
            require(host.read(file.getText) == original, "code actions changed the source")
            actions.find(_.title == title) match {
              case None => "disagree: surrounder was not offered"
              case Some(action) if action.disabled.nonEmpty => s"disagree: ${action.disabled.get}"
              case Some(action) =>
                val actual = TestHost.applyEdits(original, action.edits).trim
                if (actual != expected) s"disagree: expected ${expected.replace("\n", "\\n")}; actual ${actual.replace("\n", "\\n")}"
                else if (HostLog.count(HostLog.Level.Error) != errors) "disagree: platform logged an error"
                else "agree"
            }
          })
        } catch {
          case e @ (NonFatal(_) | _: LinkageError) =>
            s"disagree: ${e.getClass.getSimpleName}: ${e.getMessage}"
        } finally host.closeSource(file)
      }
      println(s"$name: $result")
      name -> result
    }
    val summary = s"${results.count(_._2 == "agree")} agree, ${results.count(_._2.startsWith("disagree"))} disagree, " +
      s"${results.count(_._2.startsWith("skip"))} skip"
    Files.writeString(Path.of(report), "# Code actions oracle\n\nSurroundWithTest Scala 2/3 fixtures.\n\n" + summary +
      "\n\n" + results.map((name, result) => s"- $name: $result").mkString("\n") + "\n")
    println(summary)
    sys.exit(0)
  }
}
