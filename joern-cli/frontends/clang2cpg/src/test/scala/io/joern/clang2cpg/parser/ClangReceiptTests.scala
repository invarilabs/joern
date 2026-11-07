package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangReceiptTests extends AnyWordSpec with Matchers {
  "Native capture receipt" should {
    "record canonical tool paths while preserving a compiler invocation alias" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-receipt-").toRealPath()
      val source = root.resolve("sample.c")
      Files.writeString(source, "int f(void) {return 1;}\n")
      val compiler = Paths.get(sys.env("CLANG_CPG_COMPILER")).toRealPath()
      val plugin = Paths.get(sys.env("CLANG_CPG_PLUGIN")).toRealPath()
      val compilerAlias = Files.createSymbolicLink(root.resolve("clang-alias"), compiler)
      val pluginAlias = Files.createSymbolicLink(root.resolve("plugin-alias.so"), plugin)
      val argv = Seq(compilerAlias.toString, "-std=gnu11", source.toString)
      val command = root.resolve("command.json")
      Files.writeString(command, ujson.write(argv))
      val evidence = ClangParser.capture(Config(
        clangCommand = Some(command.toString), clangPlugin = Some(pluginAlias.toString),
        clangEvidenceDir = Some(root.resolve("evidence").toString)
      ).withInputPath(source.toString))
      val receipt = ujson.read(Files.readString(evidence.directory.resolve("receipt.json")))
      receipt("selected_command").arr.map(_.str).toSeq shouldBe argv
      receipt("tools").obj.keys.foreach { name =>
        Paths.get(name).toRealPath().toString shouldBe name
      }
      receipt("tools").obj.keySet should contain allOf (compiler.toString, plugin.toString)
      receipt("tools").obj.keySet should not contain compilerAlias.toString
      receipt("tools").obj.keySet should not contain pluginAlias.toString
    }
    "reject an invocation alias retargeted to identical bytes" in {
      val root = Files.createTempDirectory("clang-alias-change-").toRealPath()
      val first = root.resolve("first")
      val second = root.resolve("second")
      Files.writeString(first, "same bytes")
      Files.writeString(second, "same bytes")
      val alias = Files.createSymbolicLink(root.resolve("alias"), first)
      val protectedInput = ClangParser.protect(alias)
      Files.delete(alias)
      Files.createSymbolicLink(alias, second)
      intercept[IllegalArgumentException](protectedInput.validate()).getMessage should include("target changed")
    }
  }
}
