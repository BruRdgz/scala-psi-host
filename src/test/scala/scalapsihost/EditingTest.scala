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
      assertEquals(edit.getNewText, "message()")
      assertEquals(edit.getRange.getStart.getCharacter, offset - 3)
      assertEquals(edit.getRange.getEnd.getCharacter, offset)
      assertEquals(host.read(file.getText), text)
    } finally host.closeSource(file)
  }

  test("completion preserves overloads and deprecation, and inserts classes through import and constructor handlers") {
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
      for (expression <- Seq("val value: ZebraUn = ???", "val value: Any = new ZebraUn")) {
        val changed = s"package visible\nobject Lookup { $expression }"
        host.edit(file, 0, host.read(file.getTextLength), changed)
        val completed = host.completion(file, changed.indexOf("ZebraUn") + "ZebraUn".length)
        val selected = completed.getItems.asScala.find(_.getFilterText == "ZebraUnimported").getOrElse(fail(completed.toString))
        assertEquals(selected.getTextEdit.getLeft.getNewText, "ZebraUnimported")
        val imports = selected.getAdditionalTextEdits.asScala.toSeq
        assert(imports.exists(_.getNewText.contains("import hidden.ZebraUnimported")), imports)
        val edits = (imports :+ selected.getTextEdit.getLeft).sortBy(e => (e.getRange.getStart.getLine, e.getRange.getStart.getCharacter))
        assert(applyEdits(changed, edits).contains(expression.replace("ZebraUn", "ZebraUnimported")))
        assertEquals(host.read(file.getText), changed)
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
  test("completion resolve replays a versioned recipe and snippets retain the handler caret") {
    import scala.jdk.CollectionConverters.*
    val target = host.openFile(Path.of("/editing/ResolveTarget.scala"), "package imported\nclass ResolveTarget(n: Int)\n", module)
    val text = "object ResolveUser { val x = new ResolveT }"
    val file = host.openFile(Path.of("/editing/ResolveUser.scala"), text, module) match {
      case f: ScalaFile => f
      case other => fail(s"not Scala: $other")
    }
    val offset = text.indexOf("ResolveT }") + "ResolveT".length
    try {
      for (snippets <- Seq(false, true)) {
        val options = Completion.Options(snippets, true, "file:///editing/ResolveUser.scala", 1)
        val list = host.completion(file, offset, options)
        val item = list.getItems.asScala.find(_.getFilterText == "ResolveTarget").getOrElse(fail(list.toString))
        assertEquals(item.getTextEdit.getLeft.getNewText, if (snippets) "ResolveTarget($0)" else "ResolveTarget()")
        assertEquals(item.getAdditionalTextEdits, null)
        assert(item.getData != null)
        val resolved = host.resolveCompletion(file, item, 1)
        assert(resolved.getAdditionalTextEdits.asScala.exists(_.getNewText.contains("import imported.ResolveTarget")))
        intercept[com.intellij.openapi.progress.ProcessCanceledException](host.resolveCompletion(file, item, 2))
      }
      val item = host.completion(file, offset, Completion.Options(resolve = true)).getItems.asScala
        .find(_.getFilterText == "ResolveTarget").get
      host.edit(file, 0, 0, "// changed\n")
      intercept[com.intellij.openapi.progress.ProcessCanceledException](host.resolveCompletion(file, item, 0))
    } finally { host.closeSource(file); host.closeSource(target) }
  }

  test("string, backtick and keyword handlers preserve surrounding text and their caret") {
    import scala.jdk.CollectionConverters.*
    val cases = Seq(
      ("object Strings { val member = 1; val s = \"$mem<caret>\" }", "member", "object Strings { val member = 1; val s = s\"$member<caret>\" }"),
      ("object Strings { object value { val member = 1 }; val s = s\"$value.mem<caret>\" }", "member",
        "object Strings { object value { val member = 1 }; val s = s\"${value.member<caret>}\" }"),
      ("object Ticks { def `type`(): Unit = (); Ticks.`ty<caret>` }", "`type`", "object Ticks { def `type`(): Unit = (); Ticks.`type`()<caret> }"),
      ("object Keywords {\n  val n = 1\n  n mat<caret>\n}", "match", "object Keywords {\n  val n = 1\n  n match {\n    case <caret>\n  }\n}")
    )
    for ((marked, lookup, expected) <- cases) {
      val offset = marked.indexOf("<caret>")
      val text = marked.replace("<caret>", "")
      val file = host.openFile(Path.of("/editing/Handler.scala"), text, module) match {
        case f: ScalaFile => f
        case other => fail(s"not Scala: $other")
      }
      try {
        val items = host.completion(file, offset, Completion.Options(snippets = true)).getItems.asScala
        val item = items.find(_.getFilterText == lookup).getOrElse(fail(items.toString))
        val primary = item.getTextEdit.getLeft
        val replacement = primary.getNewText.replace("$0", "<caret>").replace("\\$", "$").replace("\\}", "}").replace("\\\\", "\\")
        val edits = (Option(item.getAdditionalTextEdits).map(_.asScala.toSeq).getOrElse(Seq.empty) :+
          new org.eclipse.lsp4j.TextEdit(primary.getRange, replacement)).sortBy(e => (e.getRange.getStart.getLine, e.getRange.getStart.getCharacter))
        assertEquals(applyEdits(text, edits), expected)
        assertEquals(host.read(file.getText), text)
      } finally host.closeSource(file)
    }
  }

  test("prefix binding and constructor adjustment advertise their final primary edit before resolve") {
    import scala.jdk.CollectionConverters.*
    val manager = com.intellij.psi.codeStyle.CodeStyleSettingsManager.getInstance(host.project)
    val settings = host.read(manager.getCurrentSettings.clone())
    settings.getCustomSettings(classOf[org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings])
      .setImportsWithPrefix(Array("scala.collection.mutable._"))
    for (expression <- Seq("val value: ArrayBuff = ???", "val value = new ArrayBuff")) {
      val text = s"object PrefixUser { $expression }"
      val file = host.openFile(Path.of("/editing/PrefixUser.scala"), text, module) match {
        case f: ScalaFile => f
        case other => fail(s"not Scala: $other")
      }
      try manager.computeWithLocalSettings(settings, _ => {
        val items = host.completion(file, text.indexOf("ArrayBuff") + "ArrayBuff".length,
          Completion.Options(resolve = true)).getItems.asScala
        val item = items.find(_.getFilterText == "ArrayBuffer").getOrElse(fail(items.toString))
        assert(item.getTextEdit.getLeft.getNewText.startsWith("mutable.ArrayBuffer"), item.toString)
        val primary = item.getTextEdit.getLeft
        val resolved = host.resolveCompletion(file, item, 0)
        assertEquals(resolved.getTextEdit.getLeft, primary)
        assert(resolved.getAdditionalTextEdits.asScala.exists(_.getNewText.contains("import scala.collection.mutable")), resolved.toString)
      }) finally host.closeSource(file)
    }
  }

  test("constructors requiring implementation UI are filtered explicitly") {
    import scala.jdk.CollectionConverters.*
    val text = "trait NeedsImplementation { def run(): Unit }; object User { new NeedsImpl }"
    val file = host.openFile(Path.of("/editing/NeedsImplementation.scala"), text, module) match {
      case f: ScalaFile => f
      case other => fail(s"not Scala: $other")
    }
    try {
      val key = "Completion.filtered.implementationDialog"
      val before = CallLog.snapshot.toMap.getOrElse(key, 0)
      val items = host.completion(file, text.lastIndexOf("NeedsImpl") + "NeedsImpl".length).getItems.asScala
      assert(!items.exists(_.getFilterText == "NeedsImplementation"), items)
      assert(CallLog.snapshot.toMap.getOrElse(key, 0) > before)
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
