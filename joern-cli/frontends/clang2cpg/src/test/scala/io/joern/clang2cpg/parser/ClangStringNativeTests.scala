package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangStringNativeTests extends AnyWordSpec with Matchers {
  private def capture(embedLegacy: Boolean): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-string-native-")
    val source = directory.resolve("sample.c")
    Files.writeString(directory.resolve("suffix.h"), "#define SUFFIX \"suffix\"\n")
    Files.writeString(source, "#include \"suffix.h\"\nconst char *read(void){return \"prefix\" SUFFIX;}\n")
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(compiler, "-std=gnu11", source.toString)))
    ClangParser.capture(Config(clangCommand = Some(command.toString), clangPlugin = Some(plugin),
      clangEvidenceDir = Some(directory.resolve("evidence").toString), embedNativeEvidence = embedLegacy)
      .withInputPath(source.toString)).document
  }

  "Compiler string-literal spelling" should {
    Seq(false, true).foreach { embedLegacy =>
      s"retain combined header-macro literal evidence with legacy payload $embedLegacy" in {
        val document = capture(embedLegacy)
        val literal = document("nodes").obj.values.find(_.obj.contains("compiler_literal_spelling")).get
        literal("compiler_literal_spelling").str shouldBe "\"prefixsuffix\""
        literal("string_literal")("unit_width").num shouldBe 1
        literal("string_literal")("units").arr.map(_.num.toInt.toChar).mkString shouldBe "prefixsuffix"
        literal("semantic_type")("kind").str shouldBe "ConstantArray"
        literal("semantic_type")("extent").num shouldBe 13
      }
    }
  }
}
