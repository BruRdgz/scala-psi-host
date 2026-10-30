package scalapsihost

import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.util.TextRange
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.util.PsiTreeUtil
import org.eclipse.lsp4j.CompletionItemKind
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import scalapsihost.TestHost.{applyEdits, choose}

import java.nio.file.Path
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

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

  // The word index follows edits: a usage added to or removed from a file is found, or no longer found.
  test("reference search finds usages in other files and follows edits") {
    val aText = "object Searched { def target: Int = 1 }\n"
    val bText = "object SearchUser { val x: Int = 1 }\n"
    val a = host.openFile(Path.of("/editing/Searched.scala"), aText, module).asInstanceOf[ScalaFile]
    val b = host.openFile(Path.of("/editing/SearchUser.scala"), bText, module).asInstanceOf[ScalaFile]
    def usages: Seq[(String, String)] = host.read {
      val target = PsiTreeUtil.findChildrenOfType(a, classOf[ScFunction]).asScala.head
      ReferencesSearch.search(target).findAll().asScala.map(ref => ref.getElement.getContainingFile.getName -> ref.getElement.getText).toSeq
    }
    try {
      assertEquals(usages, Seq.empty)

      val usage = bText.indexOf("1")
      host.edit(b, usage, usage + 1, "Searched.target")
      assertEquals(usages, Seq("SearchUser.scala" -> "Searched.target"))

      host.edit(b, usage, usage + "Searched.target".length, "1")
      assertEquals(usages, Seq.empty)
    } finally {
      host.closeSource(a)
      host.closeSource(b)
    }
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

  private def scalaFile(name: String, text: String): ScalaFile =
    host.openFile(Path.of(s"/editing/$name.scala"), text, module) match {
      case f: ScalaFile => f
      case other => fail(s"not Scala: $other")
    }

  test("completion renders member signatures, and its handler adds the parentheses once chosen") {
    val text = "object Complete { val emoji = \"😀\"; def message(n: Int): String = \"hi\"; Complete.mes }"
    val file = scalaFile("Complete", text)
    try {
      val offset = text.indexOf("Complete.mes") + "Complete.mes".length
      val result = host.completion(file, offset)
      val item = result.items.find(_.lookup == "message").getOrElse(fail(result.toString))
      assertEquals(item.kind, CompletionItemKind.Method)
      assert(item.tail.exists(_.contains("Int")) && item.typeText.contains("String"), item.toString)
      assertEquals(item.prefix, new TextRange(offset - 3, offset))
      assert(item.hasHandler, item.toString)
      assertEquals(host.read(file.getText), text)
      assert(choose(file, offset, "message").contains("Complete.message(<caret>)"))
    } finally host.closeSource(file)
  }

  test("completion after `new` presents library Java classes") {
    val text = "object AfterNew { val t = new Threa; val l = new java.util.ArrayLi }"
    val file = scalaFile("AfterNew", text)
    try {
      for ((prefix, name) <- Seq("new Threa" -> "Thread", "new java.util.ArrayLi" -> "ArrayList")) {
        val result = host.completion(file, text.indexOf(prefix) + prefix.length)
        assert(result.items.exists(_.lookup == name), s"$prefix: $result")
      }
    } finally host.closeSource(file)
  }

  test("completion preserves overloads and deprecation, and imports a chosen class") {
    val hidden = host.openFile(Path.of("/editing/Hidden.scala"), "package hidden\nclass ZebraUnimported", module)
    val text = "package visible\nobject Lookup { @deprecated(\"old\", \"1\") def message(n: Int): String = \"\"; " +
      "def message(n: String): Int = 0; Lookup.mes }"
    val file = scalaFile("Lookup", text)
    try {
      val offset = text.indexOf("Lookup.mes") + "Lookup.mes".length
      val completed = host.completion(file, offset)
      val limited = Completion.complete(file, offset, limit = 1)
      assertEquals(limited.items, completed.items.take(1))
      assert(limited.incomplete)
      val unlimited = Completion.complete(file, offset, limit = Int.MaxValue)
      assertEquals(unlimited, completed)
      val items = completed.items.filter(_.lookup == "message")
      assertEquals(items.size, 2)
      assertEquals(items.map(_.identity).distinct.size, 2)
      assertEquals(items.count(_.deprecated), 1)
      for (expression <- Seq("val value: ZebraUn = ???", "val value: Any = new ZebraUn")) {
        val changed = s"package visible\nobject Lookup { $expression }"
        host.edit(file, 0, host.read(file.getTextLength), changed)
        val chosen = choose(file, changed.indexOf("ZebraUn") + "ZebraUn".length, "ZebraUnimported")
        assert(chosen.contains("import hidden.ZebraUnimported"), chosen)
        assert(chosen.replace("<caret>", "").contains(expression.replace("ZebraUn", "ZebraUnimported")), chosen)
      }
    } finally {
      host.closeSource(file)
      host.closeSource(hidden)
    }
  }

  test("completion ranks a local before a named argument, whose handler adds the assignment") {
    val text = "object Named { def use = { val namelocal = \"s\"; " +
      "def printName(nameParam: String = \"Unknown\"): Unit = (); printName(name) } }"
    val file = scalaFile("Named", text)
    try {
      val offset = text.indexOf("printName(name)") + "printName(name".length
      val names = host.completion(file, offset).items.map(_.lookup)
      assert(names.contains("namelocal") && names.contains("nameParam"), names)
      assert(names.indexOf("namelocal") < names.indexOf("nameParam"), names)
      assert(choose(file, offset, "nameParam").contains("printName(nameParam = <caret>)"))
    } finally host.closeSource(file)
  }

  test("a chosen constructor gets its import and the caret between its parentheses") {
    val target = host.openFile(Path.of("/editing/ConstructorTarget.scala"), "package imported\nclass ConstructorTarget(n: Int)\n", module)
    val text = "object ConstructorUser { val x = new ConstructorT }"
    val file = scalaFile("ConstructorUser", text)
    try {
      val chosen = choose(file, text.indexOf("ConstructorT }") + "ConstructorT".length, "ConstructorTarget")
      assert(chosen.contains("new ConstructorTarget(<caret>)"), chosen)
      assert(chosen.contains("import imported.ConstructorTarget"), chosen)
    } finally {
      host.closeSource(file)
      host.closeSource(target)
    }
  }

  test("string, backtick and keyword handlers preserve surrounding text and their caret") {
    val cases = Seq(
      ("object Strings { val member = 1; val s = \"$mem<caret>\" }", "member", "object Strings { val member = 1; val s = s\"$member<caret>\" }"),
      ("object Strings { object value { val member = 1 }; val s = s\"$value.mem<caret>\" }", "member",
        "object Strings { object value { val member = 1 }; val s = s\"${value.member<caret>}\" }"),
      ("object Ticks { def `type`(): Unit = (); Ticks.`ty<caret>` }", "`type`", "object Ticks { def `type`(): Unit = (); Ticks.`type`()<caret> }"),
      ("object Keywords {\n  val n = 1\n  n mat<caret>\n}", "match", "object Keywords {\n  val n = 1\n  n match {\n    case <caret>\n  }\n}")
    )
    for ((marked, lookup, expected) <- cases) {
      val file = scalaFile("Handler", marked.replace("<caret>", ""))
      try assertEquals(choose(file, marked.indexOf("<caret>"), lookup), expected)
      finally host.closeSource(file)
    }
  }

  test("a chosen class is imported by its package prefix when the code style asks for it") {
    val manager = com.intellij.psi.codeStyle.CodeStyleSettingsManager.getInstance(host.project)
    val settings = host.read(manager.getCurrentSettings.clone())
    settings.getCustomSettings(classOf[org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings])
      .setImportsWithPrefix(Array("scala.collection.mutable._"))
    for (expression <- Seq("val value: ArrayBuff = ???", "val value = new ArrayBuff")) {
      val text = s"object PrefixUser { $expression }"
      val file = scalaFile("PrefixUser", text)
      try manager.computeWithLocalSettings(settings, _ => {
        val chosen = choose(file, text.indexOf("ArrayBuff") + "ArrayBuff".length, "ArrayBuffer")
        assert(chosen.contains("mutable.ArrayBuffer"), chosen)
        assert(chosen.contains("import scala.collection.mutable"), chosen)
      }) finally host.closeSource(file)
    }
  }

  test("completion offers an abstract class after `new`, though its handler's dialog cannot run") {
    val text = "trait NeedsImplementation { def run(): Unit }; object User { new NeedsImpl }"
    val file = scalaFile("NeedsImplementation", text)
    try {
      val chosen = choose(file, text.lastIndexOf("NeedsImpl") + "NeedsImpl".length, "NeedsImplementation")
      assert(chosen.contains("new NeedsImplementation"), chosen)
    } finally host.closeSource(file)
  }

  test("word-aligned character diffs separate imports and preserve UTF-16 boundaries") {
    val original = "object Diff { val n = 1 }\n"
    val imported = "import example.Value\n\n"
    val changed = imported + original.replace("1", "2")
    val edits = ChangeEngine.diff(original, changed)
    assertEquals(edits.map(_.text), Seq(imported, "2"))
    assertEquals(applyEdits(original, edits), changed)
    assertEquals(ChangeEngine.diff(original, original), Seq.empty)
    val supplementary = "// 😀\nval x=1\n"
    val replacement = "// 🦊\nval x = 1\n"
    val unicodeEdits = ChangeEngine.diff(supplementary, replacement)
    assertEquals(unicodeEdits.head.range, new TextRange(3, 5))
    assertEquals(applyEdits(supplementary, unicodeEdits), replacement)
  }

  test("copy edits reconstruct exactly without changing source PSI, document or index") {
    val text = "object CopyOriginal { val emoji = \"😀\"; val number = 1 }\n"
    val file = host.openFile(Path.of("/editing/CopyOriginal.scala"), text, module) match {
      case f: ScalaFile => f
      case other => fail(s"not Scala: $other")
    }
    try {
      val documents = com.intellij.psi.PsiDocumentManager.getInstance(host.project)
      val stamp = host.read(documents.getDocument(file).getModificationStamp)
      val key = com.intellij.openapi.util.Key.create[String]("change-engine.copyable")
      host.read(file.putCopyableUserData(key, "copied"))
      val (changed, _) = ChangeEngine.run(file) { copy =>
        assert(!copy.isPhysical)
        assertEquals(copy.getCopyableUserData(key), "copied")
        assertEquals(org.jetbrains.plugins.scala.project.ScalaFeatures.forPsiOrDefault(copy).isScala3,
          org.jetbrains.plugins.scala.project.ScalaFeatures.forPsiOrDefault(file).isScala3)
        val document = copy.getViewProvider.getDocument
        val start = document.getText.indexOf("😀")
        document.replaceString(start, start + "😀".length, "🦊")
        documents.commitDocument(document)
        assert(copy.getText.contains("🦊"), copy.getText)
        document.insertString(document.getTextLength, "// done\n")
      }
      assertEquals(changed.text, text.replace("😀", "🦊") + "// done\n")
      assertEquals(applyEdits(text, changed.edits), changed.text)
      assertEquals(host.read(file.getText), text)
      assertEquals(host.read(documents.getDocument(file).getText), text)
      assertEquals(host.read(documents.getDocument(file).getModificationStamp), stamp)
      host.read {
        val classes = com.intellij.psi.search.PsiShortNamesCache.getInstance(host.project)
          .getClassesByName("CopyOriginal", com.intellij.psi.search.GlobalSearchScope.allScope(host.project))
        assert(classes.nonEmpty)
        assert(classes.forall(_.getContainingFile eq file), classes.toSeq)
      }
    } finally host.closeSource(file)
  }

  test("IntelliJ formatting returns minimal edits with request indentation") {
    val text = "object FormatMe {\nval emoji=\"😀\"\nval n=1+2\n}\n"
    val file = host.openFile(Path.of("/editing/FormatMe.scala"), text, module) match {
      case f: ScalaFile => f
      case other => fail(s"not Scala: $other")
    }
    try {
      val result = ChangeEngine.format(file, None, 4, useTabs = false)
      assertEquals(result.text, "object FormatMe {\n    val emoji = \"😀\"\n    val n = 1 + 2\n}\n")
      assertEquals(applyEdits(text, result.edits), result.text)
      assertEquals(host.read(file.getText), text)
      assert(result.edits.size > 1, result.edits)
    } finally host.closeSource(file)
  }

  test("formatting a copy completes while another thread holds a read") {
    val text = "object Concurrent { val n=1+2 }\n"
    val file = scalaFile("Concurrent", text)
    val started = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val reader = Future {
      HostLock.readLocked {
        started.countDown()
        release.await()
      }
    }(using ExecutionContext.global)
    try {
      assert(started.await(30, TimeUnit.SECONDS), "the read did not start")
      val formatting = Future {
        ChangeEngine.format(file, None, 2, useTabs = false)
      }(using ExecutionContext.global)
      val result = Await.result(formatting, Duration(5, "s"))
      assertEquals(result.text, "object Concurrent {\n  val n = 1 + 2\n}\n")
      assertEquals(applyEdits(text, result.edits), result.text)
    } finally {
      release.countDown()
      Await.result(reader, Duration(30, "s"))
      host.closeSource(file)
    }
  }

  test("a newer edit cancels a copy modification and releases its read lock") {
    val file = host.openFile(Path.of("/editing/Cancelled.scala"), "object Cancelled\n", module) match {
      case f: ScalaFile => f
      case other => fail(s"not Scala: $other")
    }
    try {
      val started = new java.util.concurrent.CountDownLatch(1)
      val query = scala.concurrent.Future {
        ChangeEngine.run(file) { _ =>
          started.countDown()
          while (true) com.intellij.openapi.progress.ProgressManager.checkCanceled()
        }
      }(using scala.concurrent.ExecutionContext.global)
      assert(started.await(30, java.util.concurrent.TimeUnit.SECONDS))
      host.edit(file, 0, 0, "// newer\n")
      intercept[com.intellij.openapi.progress.ProcessCanceledException](
        scala.concurrent.Await.result(query, scala.concurrent.duration.Duration(30, "s")))
      assertEquals(host.read(file.getText), "// newer\nobject Cancelled\n")
    } finally host.closeSource(file)
  }

  test("addImportForClass changes only the import line through the copy engine") {
    val target = host.openFile(Path.of("/editing/ImportTarget.scala"), "package imported\nclass ImportTarget\n", module)
    val text = "object ImportUser { val x: ImportTarget = ??? }\n"
    val file = host.openFile(Path.of("/editing/ImportUser.scala"), text, module) match {
      case f: ScalaFile => f
      case other => fail(s"not Scala: $other")
    }
    try {
      val cls = host.read {
        com.intellij.psi.JavaPsiFacade.getInstance(host.project).findClass("imported.ImportTarget",
          com.intellij.psi.search.GlobalSearchScope.allScope(host.project))
      }
      assert(cls != null)
      val (result, _) = ChangeEngine.run(file)(_.addImportForClass(cls, null))
      assertEquals(result.edits.size, 1, result.edits)
      assertEquals(result.edits.head.range, TextRange.EMPTY_RANGE)
      assertEquals(result.text, "import imported.ImportTarget\n\n" + text)
      assertEquals(applyEdits(text, result.edits), result.text)
      assertEquals(host.read(file.getText), text)
    } finally {
      host.closeSource(file)
      host.closeSource(target)
    }
  }

}
