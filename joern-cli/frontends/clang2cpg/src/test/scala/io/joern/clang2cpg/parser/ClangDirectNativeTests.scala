package io.joern.clang2cpg.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

class ClangDirectNativeTests extends AnyWordSpec with Matchers {
  private def capture(direct: Boolean): (ujson.Value, Path, Path) = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-direct-native-")
    val source = directory.resolve("sample.c")
    val header = directory.resolve("types.h")
    Files.writeString(header, "typedef struct Box {int value;} Box;\n")
    Files.writeString(source,
      "#include \"types.h\"\n#define VALUE 7\n#if VALUE\n// retained\nint read(Box *box){return box->value+VALUE;}\n#endif\n")
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val output = directory.resolve("clang.json")
    val arguments = Seq(compiler, "-std=gnu11", "-fsyntax-only", "-Xclang", "-load", "-Xclang", plugin,
      "-Xclang", "-add-plugin", "-Xclang", "clang-cpg") ++
      Seq(output.toString, "include-comments").flatMap(value => Seq("-Xclang", "-plugin-arg-clang-cpg", "-Xclang", value)) ++
      Option.when(direct)(Seq("-Xclang", "-plugin-arg-clang-cpg", "-Xclang", "direct-cpg-only")).toSeq.flatten ++
      Seq(source.toString)
    val process = new ProcessBuilder(arguments.asJava).redirectError(directory.resolve("compiler.log").toFile).start()
    process.waitFor() shouldBe 0
    (ujson.read(Files.readString(output)), output, header)
  }

  "Explicit direct native evidence" should {
    "retain typed AST, macros, comments and dependency closure without legacy payloads" in {
      val (document, output, header) = capture(direct = true)
      document("direct_cpg_only").bool shouldBe true
      document("ast")("kind").str shouldBe "TranslationUnitDecl"
      document("nodes").obj.values.exists(node =>
        node.obj.get("semantic_type").exists(value => value("kind").str == "Record")) shouldBe true
      document("macros").arr.exists(_("name").str == "VALUE") shouldBe true
      document("macro_directives").arr.exists(_("name").str == "VALUE") shouldBe true
      document("conditional_directives").arr.map(_("condition").str).toList shouldBe List("VALUE")
      document("comments").arr.map(_("code").str).toList shouldBe List("// retained")
      document("inputs").obj.keySet should contain(header.toRealPath().toString)
      document.obj should not contain key("compiler_graph")
      document.obj should not contain key("tokens")
      Files.exists(Paths.get(output.toString + ".tokens")) shouldBe false
    }
    "keep the legacy producer payload enabled when no mode is selected" in {
      val (document, output, _) = capture(direct = false)
      document.obj should contain key("compiler_graph")
      document("tokens").arr should not be empty
      Files.exists(Paths.get(output.toString + ".tokens")) shouldBe true
    }
  }
}
