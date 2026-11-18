package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.memory.{NativeCpg, NativeGraphEvidence}
import io.shiftleft.semanticcpg.language.*
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NativeIncludedDefinitionTests extends AnyWordSpec with Matchers {
  "Explicit included definitions" should {
    "capture selected bodies and their declaration closure without importing unrelated header bodies" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "included-definitions-").toRealPath()
      val included = root.resolve("generated.inc")
      val unrelated = root.resolve("unrelated.h")
      val source = root.resolve("sample.c")
      Files.writeString(included, "static int helper(int value){static int delta=2;return value+delta;} int included_entry(int value){return helper(value);}")
      Files.writeString(unrelated, "static inline int ignored(void){return 99;}")
      Files.writeString(source, "#include \"unrelated.h\"\n#include \"generated.inc\"\nint anchor(void){return 0;}")
      val input = ClangInput(source, Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), root)
      val selection = NativeCpg.IncludedDefinition(included, NativeGraphEvidence.digest(included))
      val graph = NativeCpg.create(Seq(input), root, root.resolve("included.bin"),
        Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")), mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = true,
        includedDefinitions = Map(source -> Seq(selection))).get
      try {
        graph.method.nameExact("included_entry").isExternal.l shouldBe List(false)
        graph.method.nameExact("helper").ast.isCall.nameExact("<operator>.addition").size shouldBe 1
        graph.method.nameExact("helper").local.name.l should contain("delta")
        graph.method.nameExact("ignored").size shouldBe 0
        graph.method.nameExact("included_entry").tag.nameExact("clang.declarationIdentity").value.l shouldBe List("c:@F@included_entry")
      } finally graph.close()
      val original = NativeCpg.create(Seq(input), root, root.resolve("default.bin"),
        Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")), mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = true).get
      try original.method.nameExact("included_entry").size shouldBe 0 finally original.close()
    }
    "retain included declaration-macro physical source identity" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "included-macro-").toRealPath()
      val source = root.resolve("sample.c")
      val included = root.resolve("body.inc")
      Files.writeString(included, "#define SCALAR int\nint included_macro(void){SCALAR value=2;return value;}")
      Files.writeString(source, "#include \"body.inc\"\nint anchor(void){return 0;}")
      val input = ClangInput(source, Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), root)
      val graph = NativeCpg.create(Seq(input), root, root.resolve("macro.bin"),
        Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")), mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = true,
        includedDefinitions = Map(source -> Seq(NativeCpg.IncludedDefinition(included, NativeGraphEvidence.digest(included))))).get
      try {
        val proof = ujson.read(graph.local.nameExact("value").tag.nameExact("clang.declarationMacro").value.head)
        proof("file").str shouldBe included.toString
      } finally graph.close()
    }
    "reject stale, foreign, duplicate and unconsumed selections" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "included-invalid-").toRealPath()
      val source = root.resolve("sample.c")
      val included = root.resolve("unconsumed.inc")
      Files.writeString(source, "int anchor(void){return 0;}")
      Files.writeString(included, "int not_in_translation_unit(void){return 3;}")
      val input = ClangInput(source, Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), root)
      val selection = NativeCpg.IncludedDefinition(included, NativeGraphEvidence.digest(included))
      val choices = Seq(Map(source -> Seq(selection.copy(sha256 = "0" * 64))),
        Map(included -> Seq(selection)), Map(source -> Seq(selection, selection)), Map(source -> Seq(selection)))
      choices.zipWithIndex.foreach { case (selected, index) =>
        intercept[IllegalArgumentException] {
          NativeCpg.create(Seq(input), root, root.resolve(s"invalid-$index.bin"),
            Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")), mutable.ArrayBuffer.empty[NativeCpg.Timing],
            repository = true, includedDefinitions = selected).get
        }
      }
    }
  }
}
