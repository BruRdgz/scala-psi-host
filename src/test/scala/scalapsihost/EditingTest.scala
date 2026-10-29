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
  private def applyEdits(text: String, edits: Seq[org.eclipse.lsp4j.TextEdit]): String = {
    val document = new com.intellij.openapi.editor.impl.DocumentImpl(text, true)
    def offset(p: org.eclipse.lsp4j.Position): Int = document.getLineStartOffset(p.getLine) + p.getCharacter
    val offsets = edits.map(e => (offset(e.getRange.getStart), offset(e.getRange.getEnd), e.getNewText))
    offsets.sliding(2).foreach {
      case Seq((start, end, _), (next, _, _)) => assert(start < next && end <= next, offsets)
      case _ => ()
    }
    offsets.reverse.foldLeft(text) { case (current, (start, end, replacement)) =>
      current.substring(0, start) + replacement + current.substring(end)
    }
  }

  test("word-aligned character diffs separate imports and preserve UTF-16 boundaries") {
    val original = "object Diff { val n = 1 }\n"
    val imported = "import example.Value\n\n"
    val changed = imported + original.replace("1", "2")
    val edits = ChangeEngine.diff(original, changed)
    assertEquals(edits.map(_.getNewText), Seq(imported, "2"))
    assertEquals(applyEdits(original, edits), changed)
    assertEquals(ChangeEngine.diff(original, original), Seq.empty)
    val supplementary = "// 😀\nval x=1\n"
    val replacement = "// 🦊\nval x = 1\n"
    val unicodeEdits = ChangeEngine.diff(supplementary, replacement)
    assertEquals(unicodeEdits.head.getRange, new org.eclipse.lsp4j.Range(
      new org.eclipse.lsp4j.Position(0, 3), new org.eclipse.lsp4j.Position(0, 5)))
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
      val changed = ChangeEngine.run(file) { copy =>
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
      assertEquals(changed.snapshotStamp, stamp)
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
      val result = ChangeEngine.format(file, None, new org.eclipse.lsp4j.FormattingOptions(4, true))
      assertEquals(result.text, "object FormatMe {\n    val emoji = \"😀\"\n    val n = 1 + 2\n}\n")
      assertEquals(applyEdits(text, result.edits), result.text)
      assertEquals(host.read(file.getText), text)
      assert(result.edits.size > 1, result.edits)
    } finally host.closeSource(file)
  }

  test("a newer edit cancels a copy modification and releases its write lock") {
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
      val result = ChangeEngine.run(file)(_.addImportForClass(cls, null))
      assertEquals(result.edits.size, 1, result.edits)
      assertEquals(result.edits.head.getRange, new org.eclipse.lsp4j.Range(new org.eclipse.lsp4j.Position(0, 0), new org.eclipse.lsp4j.Position(0, 0)))
      assertEquals(result.text, "import imported.ImportTarget\n\n" + text)
      assertEquals(applyEdits(text, result.edits), result.text)
      assertEquals(host.read(file.getText), text)
    } finally {
      host.closeSource(file)
      host.closeSource(target)
    }
  }

}
