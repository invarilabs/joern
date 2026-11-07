package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangLinkageNativeTests extends AnyWordSpec with Matchers {
  "Native function linkage" should {
    "distinguish external and static functions using compiler visibility" in {
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-linkage-native-")
      val source = directory.resolve("sample.c")
      Files.writeString(source, "int external(int); static int local(int value){return value;} int read(int value){return external(local(value));}\n")
      val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
      val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
      val command = directory.resolve("command.json")
      Files.writeString(command, ujson.write(Seq(compiler, "-std=c11", source.toString)))
      val document = ClangParser.capture(Config(
        clangCommand = Some(command.toString), clangPlugin = Some(plugin),
        clangEvidenceDir = Some(directory.resolve("evidence").toString), embedNativeEvidence = false
      ).withInputPath(source.toString)).document
      val functions = document("nodes").obj.values.filter(_.obj.contains("externally_visible")).toSeq
      Seq("external", "read").foreach { name =>
        val function = functions.find(_("name").str == name).get
        function("externally_visible").bool shouldBe true
        function("usr").str should not be empty
        function("semantic_type")("kind").str shouldBe "FunctionProto"
      }
      functions.find(_("name").str == "local").get("externally_visible").bool shouldBe false
    }
  }
}
