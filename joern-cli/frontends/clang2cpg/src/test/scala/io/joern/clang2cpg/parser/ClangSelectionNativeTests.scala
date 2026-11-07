package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangSelectionNativeTests extends AnyWordSpec with Matchers {
  private def capture(sourceText: String, headerText: String = ""): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-selection-native-")
    val source = directory.resolve("sample.c")
    Files.writeString(source, sourceText)
    Files.writeString(directory.resolve("types.h"), headerText)
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(compiler, "-std=c11", source.toString)))
    ClangParser.capture(Config(clangCommand = Some(command.toString), clangPlugin = Some(plugin),
      clangEvidenceDir = Some(directory.resolve("evidence").toString), embedNativeEvidence = false)
      .withInputPath(source.toString)).document
  }

  "Compiler-native declaration and selection evidence" should {
    "bound header comma-declarations to individual declarators while retaining full coordinates" in {
      val document = capture("#include \"types.h\"\nint read(void){return third;}\n",
        "extern int first,second,third;\n")
      val nodes = document("nodes").obj.values.toSeq
      Seq("first", "second", "third").foreach { name =>
        val node = nodes.find(_.obj.get("name").exists(_.str == name)).get
        node("code").str shouldBe name
        node("semantic_type")("kind").str shouldBe "Int"
        node("declaration_range")("offset").num shouldBe 0
        node("declaration_range").obj should not contain key("code")
      }
    }
    "retain the original main-file comma-declaration source ranges" in {
      val document = capture("extern int first,second; int read(void){return second;}\n")
      val second = document("nodes").obj.values.find(_.obj.get("name").exists(_.str == "second")).get
      second("code").str shouldBe "extern int first,second"
      second.obj should not contain key("declaration_range")
    }
    "identify the selected association without evaluating controllers or unselected calls" in {
      val document = capture("int side(void); int read(int value){return _Generic((value++), int: 7, default: side());}\n")
      val nodes = document("nodes").obj
      val selection = nodes.values.find(_.obj.contains("generic_selection")).get("generic_selection")
      selection("controlling_expression_unevaluated").bool shouldBe true
      selection("selected_association").num shouldBe 0
      nodes(selection("selected_expression").str)("code").str shouldBe "7"
      nodes(selection("controlling_expression").str)("code").str shouldBe "(value++)"
      nodes.values.exists(_.obj.get("code").exists(_.str == "side()")) shouldBe true
    }
    "identify a selected default expression directly from compiler association identity" in {
      val document = capture("int side(void); int read(double value){return _Generic(value, int: side(), default: 9);}\n")
      val nodes = document("nodes").obj
      val selection = nodes.values.find(_.obj.contains("generic_selection")).get("generic_selection")
      selection("selected_association").num shouldBe 1
      nodes(selection("selected_expression").str)("code").str shouldBe "9"
    }
  }
}
