package scalapsihost

import com.intellij.psi.search.EverythingGlobalScope
import org.jetbrains.plugins.scala.caches.ScalaShortNamesCacheManager
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass

/** Library binaries reach the plugin's symbol lookups through its own decompiler, stub builder and indexers. */
class LibraryIndexTest extends munit.FunSuite {

  test("indexes scala-library without refusing any class") {
    val report = TestHost.libraryReport
    assertEquals(report.refused, Seq.empty)
    assert(report.scalaIndexed > 0, report)
  }

  test("the plugin's class lookup finds a library class in the host index") {
    TestHost.libraryReport
    val project = TestHost.host.project
    val option = ScalaShortNamesCacheManager.getInstance(using project)
      .getClassByFQName("scala.Option", new EverythingGlobalScope(project))
    option match {
      case c: ScClass => assertEquals(c.qualifiedName, "scala.Option")
      case other => fail(s"expected the decompiled ScClass scala.Option, got $other")
    }
  }

  // The test host attaches no JDK sources: a JDK class is its own navigation element, and its text is decompiled.
  test("a Java library class without sources has decompiled text, with its declarations in it") {
    TestHost.libraryReport
    val host = TestHost.host
    host.read {
      val list = com.intellij.psi.JavaPsiFacade.getInstance(host.project)
        .findClass("java.util.ArrayList", new EverythingGlobalScope(host.project))
      val target = list.getNavigationElement
      val text = target.getContainingFile.getText
      assert(text.startsWith("ArrayList", target.getTextOffset), text.substring(target.getTextOffset, target.getTextOffset + 40))
    }
  }
}
