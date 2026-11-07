package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangTypeTraitNativeTests extends AnyWordSpec with Matchers {
  private def capture(code: String, cpp: Boolean = false): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-type-trait-native-")
    val source = directory.resolve(if (cpp) "sample.cpp" else "sample.c")
    Files.writeString(source, code)
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(compiler, if (cpp) "-std=c++17" else "-std=gnu11", source.toString)))
    ClangParser.capture(Config(clangCommand = Some(command.toString), clangPlugin = Some(plugin),
      clangEvidenceDir = Some(directory.resolve("evidence").toString), embedNativeEvidence = false)
      .withInputPath(source.toString)).document
  }

  "Native evaluated type traits" should {
    "preserve compiler compatibility results and structural alias operand types" in {
      val document = capture("typedef int Count; int read(void){return __builtin_types_compatible_p(Count,int)+__builtin_types_compatible_p(Count,double);}\n")
      val nodes = document("nodes").obj.values.filter(_.obj.contains("type_trait")).toSeq.sortBy(_("offset").num)
      nodes.size shouldBe 2
      nodes.map(_("type_trait")("value").bool) shouldBe Seq(true, false)
      nodes.foreach { node =>
        node("semantic_type")("kind").str shouldBe "Int"
        val traitEvidence = node("type_trait")
        traitEvidence("name").str should not be empty
        traitEvidence("kind").num should be >= 0.0
        traitEvidence("operands_unevaluated").bool shouldBe true
        traitEvidence("arguments").arr.head("spelling").str shouldBe "Count"
        traitEvidence("arguments").arr.head("semantic_type")("kind").str shouldBe "Int"
      }
      nodes.last("type_trait")("arguments").arr.last("semantic_type")("kind").str shouldBe "Double"
    }
    "use the same compiler API for C++ traits" in {
      val document = capture("struct A {}; bool read(){return __is_class(A);}\n", cpp = true)
      val node = document("nodes").obj.values.find(_.obj.contains("type_trait")).get
      node("type_trait")("value").bool shouldBe true
      node("type_trait")("arguments").arr.head("semantic_type")("kind").str shouldBe "Record"
      node("semantic_type")("kind").str shouldBe "Bool"
    }
    "reject unresolved dependent traits rather than inventing a constant result" in {
      intercept[IllegalArgumentException] {
        capture("template<class T> bool read(){return __is_class(T);}\n", cpp = true)
      }.getMessage should include("Clang frontend failed")
    }
  }
}
