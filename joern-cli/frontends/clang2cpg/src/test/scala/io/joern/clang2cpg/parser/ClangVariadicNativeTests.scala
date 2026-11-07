package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangVariadicNativeTests extends AnyWordSpec with Matchers {
  private def descendants(node: ujson.Value): Seq[ujson.Value] =
    Seq(node) ++ node.obj.get("inner").toSeq.flatMap(_.arr.toSeq.flatMap(descendants))

  "Native variadic extraction evidence" should {
    "retain compiler result types and exact va-list operand identities for every requested value shape" in {
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-variadic-native-")
      val source = directory.resolve("sample.c")
      Files.writeString(source,
        "#include <stdarg.h>\nstruct Pair {int value;};\n" +
        "int read(int count,...){va_list ap;va_start(ap,count);int number=va_arg(ap,int);" +
        "double real=va_arg(ap,double);int *pointer=va_arg(ap,int*);struct Pair pair=va_arg(ap,struct Pair);" +
        "va_end(ap);return number+pair.value;}\n")
      val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
      val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
      val command = directory.resolve("command.json")
      Files.writeString(command, ujson.write(Seq(compiler, "-std=gnu11", source.toString)))
      val document = ClangParser.capture(Config(clangCommand = Some(command.toString), clangPlugin = Some(plugin),
        clangEvidenceDir = Some(directory.resolve("evidence").toString), embedNativeEvidence = false)
        .withInputPath(source.toString)).document
      val extractions = descendants(document("ast")).filter(_.obj.get("kind").exists(_.str == "VAArgExpr"))
      extractions.size shouldBe 4
      val nodes = document("nodes").obj
      extractions.map(node => nodes(node("id").str)("semantic_type")("kind").str).toList shouldBe
        List("Int", "Double", "Pointer", "Record")
      extractions.foreach { extraction =>
        val operands = extraction("inner").arr
        operands.size shouldBe 1
        val operand = nodes(operands.head("id").str)
        operand("semantic_type")("kind").str shouldBe "Pointer"
        nodes(extraction("id").str)("type").str should not be empty
      }
    }
  }
}
