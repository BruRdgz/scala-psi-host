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
}
