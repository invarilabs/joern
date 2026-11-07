package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.memory.{ClangNative, NativeCpg, NativeValue}
import io.shiftleft.semanticcpg.language.*
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NativeMacroSharingTests extends AnyWordSpec with Matchers {
  "Repository macro provenance" should {
    "share header facts while retaining every ordered compiler occurrence for each translation unit" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "macro-sharing-")
      Files.writeString(root.resolve("common.h"), "#define HEADER_VALUE 17\nenum { header_constant = HEADER_VALUE };\n")
      val inputs = Seq("left", "right").map { name =>
        val source = root.resolve(name + ".c")
        Files.writeString(source, "#include \"common.h\"\nint " + name + "(void){return HEADER_VALUE + header_constant;}\n")
        ClangInput(source, Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), root)
      }
      val library = Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY"))
      val native = ClangNative.load(library)
      val expected = inputs.map(input => input.source.toString ->
        native.parse(input.command, input.directory)("macros").arr.map(NativeValue.write).toList).toMap
      val graph = NativeCpg.create(inputs, root, root.resolve("shared.bin"), library,
        mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = true).get
      try {
        val contracts = graph.tag.nameExact("clang.translationUnit").value.map(ujson.read(_)).toList
        contracts.size shouldBe 2
        contracts.foreach { contract =>
          contract.obj.contains("macro_sites") shouldBe false
          contract("macro_sites_storage").str shouldBe "shared-tags-sha256-v1"
          val recovered = contract("macro_site_refs").arr.map { reference =>
            val matches = graph.tag.nameExact("clang.macroSite:" + reference.str).toList
            matches.size shouldBe 1
            matches.head.value
          }.toList
          recovered shouldBe expected(contract("source").str)
        }
        val headerFacts = graph.tag.name("clang.macroSite:.*").filter { fact =>
          ujson.read(fact.value)("file").str == root.resolve("common.h").toString
        }.toList
        headerFacts.size shouldBe 1
        headerFacts.head._taggedByIn.size shouldBe 2
      } finally graph.close()
    }
  }
}
