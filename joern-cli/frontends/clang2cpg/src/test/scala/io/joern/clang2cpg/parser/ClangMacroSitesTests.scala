package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.clang.ClangMacroSites
import io.joern.clang2cpg.parser.memory.{NativeValue, NObj, NStr}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClangMacroSitesTests extends AnyWordSpec with Matchers {
  "Compiler macro sharing" should {
    "retain repeated occurrences and distinct complete records in their original order" in {
      val shared = new ClangMacroSites
      val first = NObj("name" -> "VALUE", "code" -> "VALUE", "offset" -> 4)
      val changed = NObj("name" -> "VALUE", "code" -> "VALUE", "offset" -> 7)
      val references = shared.register(Seq(first, changed, first))
      references.map(value => shared.value(value.str)) shouldBe Seq(first, changed, first).map(NativeValue.write)
      references.head shouldBe references.last
      references.head should not be references(1)
    }
    "fail on missing references and invalid records instead of supplying empty evidence" in {
      val shared = new ClangMacroSites
      intercept[IllegalArgumentException] { shared.value("missing") }.getMessage should include("missing")
      intercept[IllegalArgumentException] { shared.register(Seq(NStr("invalid"))) }.getMessage should include("object")
    }
  }
}
