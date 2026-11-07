package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangAssemblyNativeTests extends AnyWordSpec with Matchers {
  private def capture(code: String): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-assembly-native-")
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

  "Native GCC assembly evidence" should {
    "preserve read-write output, named input, exact constraints and clobbers" in {
      val document = capture("int read(int value,int input){asm volatile(\"addl %[rhs], %[lhs]\" : [lhs] \"+r\"(value) : [rhs] \"r\"(input) : \"cc\",\"memory\");return value;}\n")
      val nodes = document("nodes").obj
      val assembly = nodes.values.find(_.obj.contains("gcc_asm")).get("gcc_asm")
      assembly("template").str shouldBe "addl %[rhs], %[lhs]"
      assembly("dialect").str shouldBe "gcc"
      assembly("result_kind").str shouldBe "statement"
      assembly("result_type")("kind").str shouldBe "Void"
      assembly("volatile").bool shouldBe true
      assembly("simple").bool shouldBe false
      val output = assembly("outputs").arr.head
      output("name").str shouldBe "lhs"
      output("constraint").str shouldBe "+r"
      output("read_write").bool shouldBe true
      output("semantic_type")("kind").str shouldBe "Int"
      nodes(output("expression").str)("code").str shouldBe "value"
      val input = assembly("inputs").arr.head
      input("constraint").str shouldBe "r"
      nodes(input("expression").str)("code").str shouldBe "input"
      assembly("clobbers").arr.map(_.str).toList shouldBe List("cc", "memory")
    }
    "preserve ordinary output and basic assembly flags" in {
      val document = capture("int read(int input){int result;asm(\"movl %1, %0\" : \"=r\"(result) : \"r\"(input));asm(\"nop\");return result;}\n")
      val assemblies = document("nodes").obj.values.filter(_.obj.contains("gcc_asm")).map(_("gcc_asm")).toSeq
      val extended = assemblies.find(assembly => !assembly("simple").bool).get
      extended("outputs").arr.head("read_write").bool shouldBe false
      val basic = assemblies.find(_("simple").bool).get
      basic("template").str shouldBe "nop"
      basic("outputs").arr shouldBe empty
      basic("inputs").arr shouldBe empty
    }
    "retain explicit compiler label identities for assembly goto" in {
      val document = capture("int read(void){asm goto(\"jmp %l0\" : : : : finished);return 0;finished:return 1;}\n")
      val assembly = document("nodes").obj.values.find(_.obj.contains("gcc_asm")).get("gcc_asm")
      assembly("goto").bool shouldBe true
      val label = assembly("labels").arr.head
      label("name").str shouldBe "finished"
      label("declaration").str should not be empty
      document("nodes").obj should contain key(label("expression").str)
      val labelNode = document("nodes")(label("expression").str)
      labelNode("semantic_type")("kind").str shouldBe "Pointer"
      labelNode("label_declaration").str shouldBe label("declaration").str
      labelNode("code").str shouldBe "finished"
      labelNode("file").str should endWith("sample.c")
    }
  }
}
