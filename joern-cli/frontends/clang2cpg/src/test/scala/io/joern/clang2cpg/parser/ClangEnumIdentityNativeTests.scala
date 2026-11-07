package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangEnumIdentityNativeTests extends AnyWordSpec with Matchers {
  private def capture(code: String): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-enum-identity-")
    val source = directory.resolve("sample.c")
    Files.writeString(source, code)
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(compiler, "-std=gnu11", source.toString)))
    ClangParser.capture(Config(clangCommand = Some(command.toString), clangPlugin = Some(plugin),
      clangEvidenceDir = Some(directory.resolve("evidence").toString), embedNativeEvidence = false)
      .withInputPath(source.toString)).document
  }
  "Compiler enum identities" should {
    "disambiguate sibling anonymous enums with colliding USRs and preserve typed uses" in {
      val document = capture("int read(void){{enum {CHECK=1} first=CHECK;} {enum {CHECK=1} second=CHECK;}return 0;}\n")
      val nodes = document("nodes").obj.values.toSeq
      val variables = Seq("first", "second").map(name => nodes.find(_.obj.get("name").exists(_.str == name)).get)
      val types = variables.map(_("semantic_type"))
      types.map(_("declaration_usr").str).distinct.size shouldBe 1
      types.map(_("declaration_identity").str).distinct.size shouldBe 2
      types.map(_("canonical_identity").str).distinct.size shouldBe 2
      types.foreach { enumType =>
        enumType("canonical_identity").str shouldBe "clang:" + enumType("declaration_identity").str
        nodes.exists(node => node.obj.get("type").contains(enumType("canonical_identity")) &&
          node.obj.get("semantic_type").contains(enumType) && !node.obj.contains("storage_duration")) shouldBe true
      }
    }
    "preserve the original identity for unique and redeclared named enums" in {
      val document = capture("enum State; enum State {READY=1}; enum State current=READY;\n")
      val nodes = document("nodes").obj.values.filter(_.obj.get("semantic_type").exists(_("kind").str == "Enum")).toSeq
      nodes should not be empty
      nodes.foreach { node =>
        val enumType = node("semantic_type")
        enumType("canonical_identity").str shouldBe "clang:" + enumType("declaration_usr").str
        enumType.obj should not contain key("declaration_identity")
      }
    }
  }
}
