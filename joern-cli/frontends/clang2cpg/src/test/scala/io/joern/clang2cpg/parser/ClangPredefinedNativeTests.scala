package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangPredefinedNativeTests extends AnyWordSpec with Matchers {
  private def capture(code: String, cpp: Boolean = false): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-predefined-native-")
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
  private def text(node: ujson.Value): String = {
    node("string_literal")("unit_width").num shouldBe 1
    node("string_literal")("units").arr.map(_.num.toInt.toChar).mkString
  }

  "Compiler predefined expressions" should {
    "retain exact compiler strings for C function and pretty-function expressions" in {
      val document = capture("const char *read(int choice){return choice ? __func__ : __PRETTY_FUNCTION__;}\n")
      val predefined = document("nodes").obj.values.filter(_.obj.contains("predefined_name")).toSeq
      predefined.size shouldBe 2
      val simple = predefined.find(_("predefined_name").str == "__func__").get
      text(simple) shouldBe "read"
      simple("predefined_literal_spelling").str shouldBe "\"read\""
      text(predefined.find(_("predefined_name").str == "__PRETTY_FUNCTION__").get) shouldBe "const char *read(int)"
      predefined.foreach(_("semantic_type")("kind").str shouldBe "ConstantArray")
    }
    "retain compiler-qualified C++ names without reconstructing source signatures" in {
      val document = capture("namespace scope {const char *read(){return __PRETTY_FUNCTION__;}}\n", cpp = true)
      val predefined = document("nodes").obj.values.find(_.obj.contains("predefined_name")).get
      text(predefined) shouldBe "const char *scope::read()"
    }
    "retain compiler-provided lambda function names" in {
      val document = capture("const char *read(){return [](){return __func__;}();}\n", cpp = true)
      val predefined = document("nodes").obj.values.find(_.obj.contains("predefined_name")).get
      text(predefined) shouldBe "operator()"
    }
  }
}
