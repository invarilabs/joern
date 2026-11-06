package io.joern.c2cpg.passes.types

import io.joern.c2cpg.Config
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl

class CdtTypeEvidenceTests extends C2CpgSuite {
  "C compiler type evidence" should {
    "retain scalar field and indexed array expression types" in {
      val cpg = code("""
          |typedef struct Payload { double values[3]; int count; } Payload;
          |double read_value(const Payload *owner, int index) {
          |  return owner->values[index] + owner->count;
          |}
          |""".stripMargin).withConfig(Config().withCTypeEvidence(true))
      cpg.call.codeExact("owner->values").typeFullName.l shouldBe List("double[3]")
      cpg.call.codeExact("owner->values[index]").typeFullName.l shouldBe List("double")
      cpg.call.codeExact("owner->count").typeFullName.l shouldBe List("int")
    }

    "bind an ordinary typedef independently of the identically named C tag" in {
      val cpg = code("""
          |typedef struct Shared { int unrelated; } Logged;
          |typedef struct { double durations[3]; } Shared;
          |typedef struct Owner { Shared stats; } Owner;
          |double read_value(Owner *owner, int index) { return owner->stats.durations[index]; }
          |""".stripMargin).withConfig(Config().withCTypeEvidence(true))
      val List(stats) = cpg.member.nameExact("stats").l
      val target      = stats._evalTypeOut.flatMap(_._refOut).map(_.asInstanceOf[TypeDecl]).toList
      target.size shouldBe 1
      target.head.member.name.toSet shouldBe Set("durations")
      cpg.call.codeExact("owner->stats").typeFullName.l shouldBe List(stats.typeFullName)
      cpg.call.codeExact("owner->stats.durations[index]").typeFullName.l shouldBe List("double")
    }

    "preserve the binding through typedef chains and a local shadow" in {
      val cpg = code("""
          |typedef struct StateTag { double values[2]; } State;
          |typedef State StateAlias;
          |typedef struct { StateAlias state; } Owner;
          |double read_value(Owner *owner, int index) {
          |  typedef struct { int different; } State;
          |  return owner->state.values[index];
          |}
          |""".stripMargin).withConfig(Config().withCTypeEvidence(true))
      val List(state) = cpg.member.nameExact("state").l
      val target      = state._evalTypeOut.flatMap(_._refOut).map(_.asInstanceOf[TypeDecl]).toList
      target.size shouldBe 1
      target.head.member.name.toSet shouldBe Set("values")
      cpg.call.codeExact("owner->state.values[index]").typeFullName.l shouldBe List("double")
    }

    "retain anonymous union field types" in {
      val cpg = code("""
          |typedef struct Value { union { double number; char *text; }; } Value;
          |double read_number(Value *value) { return value->number; }
          |char *read_text(Value *value) { return value->text; }
          |""".stripMargin).withConfig(Config().withCTypeEvidence(true))
      cpg.call.codeExact("value->number").typeFullName.l shouldBe List("double")
      cpg.call.codeExact("value->text").typeFullName.l shouldBe List("char*")
    }

    "retain forward declarations without linking uses to an empty forward record" in {
      val cpg = code("""
          |struct Entry;
          |typedef struct Entry Entry;
          |struct Entry { double value; };
          |typedef struct { Entry entry; } Owner;
          |double read_value(Owner *owner) { return owner->entry.value; }
          |""".stripMargin).withConfig(Config().withCTypeEvidence(true))
      cpg.typeDecl.codeExact("struct Entry").size shouldBe 1
      val List(entry) = cpg.member.nameExact("entry").l
      val target      = entry._evalTypeOut.flatMap(_._refOut).map(_.asInstanceOf[TypeDecl]).toList
      target.size shouldBe 1
      target.head.member.name.toSet shouldBe Set("value")
      cpg.call.codeExact("owner->entry.value").typeFullName.l shouldBe List("double")
    }

    "keep record array declarations and expressions on the same element identity" in {
      val cpg = code("""
          |typedef struct Entry { double value; } Entry;
          |typedef struct { Entry entries[2]; } Owner;
          |double read_value(Owner *owner, int index) { return owner->entries[index].value; }
          |""".stripMargin).withConfig(Config().withCTypeEvidence(true))
      val List(entries) = cpg.member.nameExact("entries").l
      cpg.call.codeExact("owner->entries").typeFullName.l shouldBe List(entries.typeFullName)
      val List(elementType) = cpg.call.codeExact("owner->entries[index]").typeFullName.l
      cpg.typeDecl.fullNameExact(elementType).member.name.toSet shouldBe Set("value")
      cpg.call.codeExact("owner->entries[index].value").typeFullName.l shouldBe List("double")
    }

    "distinguish anonymous records emitted by one macro expansion" in {
      val cpg = code("""
          |#define PAIR(left, right) typedef struct { int value; } left; typedef struct { double value; } right
          |PAIR(First, Second);
          |typedef struct { First first; Second second; } Owner;
          |double read_value(Owner *owner) { return owner->second.value; }
          |""".stripMargin).withConfig(Config().withCTypeEvidence(true))
      val List(first)  = cpg.member.nameExact("first").l
      val List(second) = cpg.member.nameExact("second").l
      first.typeFullName should not be second.typeFullName
      val target = second._evalTypeOut.flatMap(_._refOut).map(_.asInstanceOf[TypeDecl]).toList
      target.size shouldBe 1
      target.head.member.nameExact("value").typeFullName.l shouldBe List("double")
    }

    "retain anonymous enum array element identity" in {
      val cpg = code("""
          |typedef enum { First = 1, Second = 2 } Choice;
          |static const Choice choices[] = { First, Second };
          |Choice read_value(int index) { return choices[index]; }
          |""".stripMargin).withConfig(Config().withCTypeEvidence(true))
      val List(element) = cpg.call.codeExact("choices[index]").l
      element.typeFullName should not be "ANY"
      cpg.typeDecl.fullNameExact(element.typeFullName).member.name.toSet shouldBe Set("First", "Second")
    }

    "leave the default frontend type representation unchanged" in {
      val cpg = code("struct Payload { int value; }; int read(struct Payload *owner) { return owner->value; }")
      cpg.call.codeExact("owner->value").typeFullName.l shouldBe List("ANY")
      cpg.parameter.nameExact("owner").typeFullName.l shouldBe List("Payload*")
    }

    "reject a field expression whose compiler type cannot be resolved" in {
      val failure = intercept[Exception] {
        code("struct Payload { int known; }; int read(struct Payload *owner) { return owner->missing; }")
          .withConfig(Config().withCTypeEvidence(true))
          .call
          .l
      }
      val causes = Iterator
        .iterate(Option[Throwable](failure))(_.flatMap(error => Option(error.getCause)))
        .takeWhile(_.nonEmpty)
        .flatten
        .map(_.toString)
        .mkString("\n")
      causes should include("C type evidence")
      causes should include("owner->missing")
    }
  }
}
