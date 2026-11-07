package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.clang.ClangDeclarationShape
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClangDeclarationShapeTests extends AnyWordSpec with Matchers {
  private def declaration(): ujson.Value = ujson.read("""{
    "id":"0xdecl", "kind":"VarDecl", "name":"value", "storageClass":"static",
    "type":{"qualType":"int"}, "loc":{"line":481}, "range":{"begin":{"offset":3}},
    "inner":[{"id":"0xinit","kind":"IntegerLiteral","value":"1",
      "type":{"qualType":"int"},"loc":{"line":481},"range":{"end":{"offset":8}}},
      {"id":"0xother","kind":"IntegerLiteral","value":"2","type":{"qualType":"int"}}]
  }""")

  "Compiler declaration shapes" should {
    "accept recursive raw location elision without modifying either representation" in {
      val previous = declaration()
      val repeated = declaration()
      repeated.obj.remove("loc")
      repeated("inner")(0).obj.remove("range")
      repeated("inner")(0)("loc") = ujson.Obj("column" -> 4)
      val previousBytes = ujson.write(previous)
      val repeatedBytes = ujson.write(repeated)
      ClangDeclarationShape.requireEquivalent(previous, repeated)
      ujson.write(previous) shouldBe previousBytes
      ujson.write(repeated) shouldBe repeatedBytes
    }
    "ignore only location fields while rejecting every semantic difference" in {
      val mutations: Seq[ujson.Value => Unit] = Seq(
        node => node("kind") = ujson.Str("ParmVarDecl"),
        node => node("type")("qualType") = ujson.Str("long"),
        node => node("inner")(0)("value") = ujson.Str("7"),
        node => node("storageClass") = ujson.Str("extern"),
        node => node("inner") = ujson.Arr.from(node("inner").arr.reverse),
        node => node("inner")(0)("id") = ujson.Str("0xchanged"),
        node => node("id") = ujson.Str("0xdifferent")
      )
      mutations.foreach { change =>
        val repeated = declaration()
        change(repeated)
        intercept[IllegalArgumentException] {
          ClangDeclarationShape.requireEquivalent(declaration(), repeated)
        }.getMessage should include("Conflicting Clang declaration")
      }
    }
    "reject missing compiler identity" in {
      val repeated = declaration()
      repeated.obj.remove("id")
      intercept[IllegalArgumentException] {
        ClangDeclarationShape.requireEquivalent(declaration(), repeated)
      }.getMessage should include("compiler identity")
    }
  }
}
