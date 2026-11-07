package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangAssertionNativeTests extends AnyWordSpec with Matchers {
  private def capture(code: String, embedLegacy: Boolean = false): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-assertion-native-")
    val source = directory.resolve("sample.c")
    Files.writeString(source, code)
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(compiler, "-std=gnu11", source.toString)))
    ClangParser.capture(Config(clangCommand = Some(command.toString), clangPlugin = Some(plugin),
      clangEvidenceDir = Some(directory.resolve("evidence").toString), embedNativeEvidence = embedLegacy)
      .withInputPath(source.toString)).document
  }

  "Native compile-time assertion evidence" should {
    "preserve typeless compiler assertion messages as non-value syntax" in {
      val document = capture("_Static_assert(sizeof(int)>=2, \"integer width\"); const char *read(void){return \"value\";}\n")
      val nodes = document("nodes").obj.values.toSeq
      val message = nodes.find(_.obj.get("code").exists(_.str == "\"integer width\"")).get
      message("non_value_role").str shouldBe "static_assert_message"
      message.obj should not contain key("type")
      message.obj should not contain key("semantic_type")
      message("string_literal")("units").arr.size shouldBe 13
      val value = nodes.find(node => node.obj.get("code").exists(_.str == "\"value\"") &&
        node.obj.contains("string_literal")).get
      value("semantic_type")("kind").str shouldBe "ConstantArray"
      value.obj should not contain key("non_value_role")
    }
    "retain certified non-value assertion messages in the default legacy graph" in {
      val document = capture("_Static_assert(1, \"compile time\"); int read(void){return 1;}\n", embedLegacy = true)
      val messages = document("compiler_graph")("nodes").obj.values.filter(node =>
        node.obj.get("non_value_role").exists(_.str == "static_assert_message")).toSeq
      messages.size shouldBe 1
      messages.head.obj should not contain key("type")
      messages.head("string_literal")("units").arr.size shouldBe 12
      document("nodes").obj.values.exists(node =>
        node.obj.get("assertion_failed").contains(ujson.Bool(false)) &&
          node.obj.get("assertion_dependent").contains(ujson.Bool(false))) shouldBe true
    }
    "retain compiler failure for a false static assertion" in {
      val failure = intercept[IllegalArgumentException] {
        capture("_Static_assert(0, \"must fail\");\n")
      }
      failure.getMessage should include("Clang frontend failed")
    }
  }
}
