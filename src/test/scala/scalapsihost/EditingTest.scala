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
      assertEquals(host.read(file.getLanguage), org.jetbrains.plugins.scala.Scala3Language.INSTANCE)
      assertEquals(errors(file.asInstanceOf[ScalaFile]), Seq.empty)
    } finally host.closeSource(file)
  }

  // As the IDE's background reads: a query checks for cancellation as it works, and an edit cancels it rather than
  // waiting for it to finish.
  test("an edit cancels the queries running meanwhile") {
    val file = host.openFile(Path.of("/editing/D.scala"), "object D\n", module)
    try {
      val started = new java.util.concurrent.CountDownLatch(1)
      val query = scala.concurrent.Future {
        host.read {
          started.countDown()
          while (true) com.intellij.openapi.progress.ProgressManager.checkCanceled()
        }
      }(using scala.concurrent.ExecutionContext.global)
      assert(started.await(30, java.util.concurrent.TimeUnit.SECONDS), "the query did not start")

      host.edit(file, 0, 0, "// edited\n")

      intercept[com.intellij.openapi.progress.ProcessCanceledException](
        scala.concurrent.Await.result(query, scala.concurrent.duration.Duration(30, "s")))
      assertEquals(host.read(file.getText), "// edited\nobject D\n")
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

      assertEquals(host.read(a.getText), "object A { def f: String = \"s\" }\n")
      assert(host.read(a.isValid), "the edited file is the same PSI file, reparsed in place")
      assertEquals(errors(b), Seq("Type mismatch. Required: Int, found: String"))
      // Nothing left the host's tables or was logged as an error on the way (both would hide problems).
      val swallowed = CallLog.snapshot.map(_._1).filter(e => e.startsWith("UNSUPPORTED ") || e.startsWith("LOGGED ERROR "))
      assertEquals(swallowed, Seq.empty)
    } finally {
      host.closeSource(a)
      host.closeSource(b)
    }
  }

  test("completion renders member signatures and replaces only the UTF-16 prefix") {
    val text = "object Complete { val emoji = \"😀\"; def message(n: Int): String = \"hi\"; Complete.mes }"
    val file = host.openFile(Path.of("/editing/Complete.scala"), text, module).asInstanceOf[ScalaFile]
    try {
      import scala.jdk.CollectionConverters.*
      val offset = text.indexOf("Complete.mes") + "Complete.mes".length
      val result = host.completion(file, offset)
      val item = result.getItems.asScala.find(_.getFilterText == "message").getOrElse(fail(result.toString))
      assertEquals(item.getKind, org.eclipse.lsp4j.CompletionItemKind.Method)
      assert(item.getDetail.contains("Int") && item.getDetail.contains("String"), item.toString)
      val edit = item.getTextEdit.getLeft
      assertEquals(edit.getNewText, "message")
      assertEquals(edit.getRange.getStart.getCharacter, offset - 3)
      assertEquals(edit.getRange.getEnd.getCharacter, offset)
      assertEquals(host.read(file.getText), text)
    } finally host.closeSource(file)
  }

  test("completion preserves overloads and deprecation, and excludes classes needing an import") {
    import scala.jdk.CollectionConverters.*
    val hidden = host.openFile(Path.of("/editing/Hidden.scala"), "package hidden\nclass ZebraUnimported", module)
    val text = "package visible\nobject Lookup { @deprecated(\"old\", \"1\") def message(n: Int): String = \"\"; " +
      "def message(n: String): Int = 0; Lookup.mes }"
    val file = host.openFile(Path.of("/editing/Lookup.scala"), text, module).asInstanceOf[ScalaFile]
    try {
      val items = host.completion(file, text.indexOf("Lookup.mes") + "Lookup.mes".length).getItems.asScala
        .filter(_.getFilterText == "message")
      assertEquals(items.size, 2)
      assertEquals(items.map(_.getDetail).distinct.size, 2)
      assertEquals(items.count(i => Option(i.getTags).exists(_.contains(org.eclipse.lsp4j.CompletionItemTag.Deprecated))), 1)
      for ((expression, filtered) <- Seq("val value: ZebraUn = ???" -> "import", "val value: Any = new ZebraUn" -> "insertion")) {
        val changed = s"package visible\nobject Lookup { $expression }"
        host.edit(file, 0, host.read(file.getTextLength), changed)
        val entry = s"Completion.filtered.$filtered"
        val before = CallLog.snapshot.toMap.getOrElse(entry, 0)
        val completed = host.completion(file, changed.indexOf("ZebraUn") + "ZebraUn".length)
        assert(!completed.getItems.asScala.exists(_.getFilterText == "ZebraUnimported"), completed.toString)
        assert(CallLog.snapshot.toMap.getOrElse(entry, 0) > before, s"the $filtered candidate was not encountered")
      }
    } finally {
      host.closeSource(file)
      host.closeSource(hidden)
    }
  }

  test("completion ranks a local before a named argument and emits its trivial assignment edit") {
    import scala.jdk.CollectionConverters.*
    val text = "object Named { def use = { val namelocal = \"s\"; " +
      "def printName(nameParam: String = \"Unknown\"): Unit = (); printName(name) } }"
    val file = host.openFile(Path.of("/editing/Named.scala"), text, module).asInstanceOf[ScalaFile]
    try {
      val items = host.completion(file, text.indexOf("printName(name)") + "printName(name".length).getItems.asScala.toSeq
      val names = items.map(_.getFilterText)
      assert(names.contains("namelocal") && names.contains("nameParam"), names)
      assert(names.indexOf("namelocal") < names.indexOf("nameParam"), names)
      assertEquals(items.find(_.getFilterText == "nameParam").get.getTextEdit.getLeft.getNewText, "nameParam = ")
    } finally host.closeSource(file)
  }
}
