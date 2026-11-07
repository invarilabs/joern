package io.joern.clang2cpg.parser.memory

import io.joern.clang2cpg.parser.clang.NativeDeclarationShape
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NativeDeclarationShapeTests extends AnyWordSpec with Matchers {
  private def declaration(): NativeValue = NObj(
    "id" -> "decl", "kind" -> "VarDecl", "name" -> "value", "storageClass" -> "static",
    "type" -> NObj("qualType" -> "int"), "loc" -> NObj("line" -> 4),
    "inner" -> NArr(NObj("id" -> "one", "kind" -> "IntegerLiteral", "value" -> "1",
      "range" -> NObj("begin" -> NObj("offset" -> 2))),
      NObj("id" -> "two", "kind" -> "IntegerLiteral", "value" -> "2")))

  "Shared typed declaration shapes" should {
    "ignore recursive loc and range fields without weakening structural equality" in {
      val original = declaration()
      val repeated = declaration()
      repeated.obj.remove("loc")
      repeated("inner")(0).obj.remove("range")
      val before = original.compact
      val after = repeated.compact
      (original == repeated) shouldBe false
      NativeDeclarationShape.requireEquivalent(original, repeated)
      original.compact shouldBe before
      repeated.compact shouldBe after
    }
    "reject all nonlocation changes including child order and identities" in {
      val changes: Seq[NativeValue => Unit] = Seq(
        value => value.obj("kind") = NStr("ParmVarDecl"),
        value => value("type").obj("qualType") = NStr("long"),
        value => value("inner")(0).obj("value") = NStr("7"),
        value => value.obj("storageClass") = NStr("extern"),
        value => value.obj("inner") = NArr.from(value("inner").arr.reverse),
        value => value("inner")(0).obj("id") = NStr("changed"),
        value => value.obj("id") = NStr("different"))
      changes.foreach { change =>
        val repeated = declaration()
        change(repeated)
        intercept[IllegalArgumentException](NativeDeclarationShape.requireEquivalent(declaration(), repeated))
          .getMessage should include("Conflicting Clang declaration")
      }
    }
    "reject absent compiler identity" in {
      val repeated = declaration()
      repeated.obj.remove("id")
      intercept[IllegalArgumentException](NativeDeclarationShape.requireEquivalent(declaration(), repeated))
        .getMessage should include("compiler identity")
    }
  }
}
