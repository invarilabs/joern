package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangPreprocessorTests extends AnyWordSpec with Matchers {
  private def capture(code: String, comments: Boolean = false): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-pp-test-")
    val source = directory.resolve("sample.c")
    Files.writeString(source, code)
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(compiler, "-std=gnu11", source.toString)))
    ClangParser.capture(Config(
      clangCommand = Some(command.toString),
      clangPlugin = Some(plugin),
      clangEvidenceDir = Some(directory.resolve("evidence").toString),
      includeComments = comments
    ).withInputPath(source.toString)).document
  }

  "Native preprocessing evidence" should {
    "retain conditional directive spelling including skipped branches" in {
      val document = capture("#if 0\n#elif 1\nint value;\n#endif\n#ifdef FLAG\n#endif\n#ifndef OTHER\n#endif\n")
      val directives = document("conditional_directives").arr
      directives.map(_("kind").str).toList shouldBe List("if", "elif", "ifdef", "ifndef")
      directives.map(_("condition").str).toList shouldBe List("0", "1", "FLAG", "OTHER")
      directives.map(_("line").num.toInt).toList shouldBe List(1, 2, 5, 7)
      directives.foreach(_("file").str.endsWith("sample.c") shouldBe true)
    }
    "emit requested comments with exact source text and positions" in {
      val document = capture("// before\nint value; /* after */\n", comments = true)
      val comments = document("comments").arr
      comments.map(_("code").str).toList shouldBe List("// before", "/* after */")
      comments.map(_("line").num.toInt).toList shouldBe List(1, 2)
      comments.foreach(comment => comment("end").num should be > comment("offset").num)
    }
    "omit comment capture unless explicitly requested" in {
      capture("// before\nint value;\n")("comments").arr shouldBe empty
    }
  }
}
