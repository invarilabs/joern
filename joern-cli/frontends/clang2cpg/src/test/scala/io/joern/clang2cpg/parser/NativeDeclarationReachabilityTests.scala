package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.memory.{NativeCpg, ClangNative}
import io.shiftleft.semanticcpg.language.*
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NativeDeclarationReachabilityTests extends AnyWordSpec with Matchers {
  "Compiler declaration closure" should {
    "retain assertion dependencies while treating compiler diagnostic text as non-value syntax" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "assertion-closure-")
      Files.writeString(root.resolve("types.h"), "struct Item { long size; }; typedef struct Item Alias;")
      val source = root.resolve("sample.c")
      Files.writeString(source, "#include \"types.h\"\n_Static_assert(sizeof(Alias)>0, \"diagnostic text\");\nconst char *read(void){return \"value\";}")
      val input = ClangInput(source, Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), root)
      val graph = NativeCpg.create(Seq(input), root, root.resolve("native.bin"),
        Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")), mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = true).get
      try {
        graph.tag.nameExact("clang.staticAssertion").size shouldBe 1
        graph.member.nameExact("size").size shouldBe 1
        graph.method.nameExact("read").size shouldBe 1
        graph.literal.codeExact("\"value\"").typeFullName.toList should not contain("ANY")
      } finally graph.close()
    }

    "retain required transitive header types and calls without copying unrelated header declarations" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "declaration-closure-")
      Files.writeString(root.resolve("common.h"), """
        struct Payload;
        typedef struct Payload Payload;
        typedef int (*Callback)(Payload *);
        struct Payload { int count; };
        typedef struct Holder { Callback callback; Payload value; } Holder;
        extern Holder shared;
        int consume(Holder *);
        int unrelated_function(int);
        struct UnrelatedRecord { int noise[32]; };
      """)
      val source = root.resolve("sample.c")
      Files.writeString(source, "#include \"common.h\"\nint source_unused(void){return 9;}\nint run(void){shared.value.count=4;return consume(&shared);}")
      val input = ClangInput(source, Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), root)
      val graph = NativeCpg.create(Seq(input), root, root.resolve("native.bin"),
        Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")), mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = true).get
      try {
        graph.method.nameExact("unrelated_function").size shouldBe 0
        graph.typeDecl.nameExact("UnrelatedRecord").size shouldBe 0
        graph.method.nameExact("source_unused").size shouldBe 1
        graph.method.nameExact("run").size shouldBe 1
        graph.method.nameExact("consume").size shouldBe 1
        graph.member.nameExact("count").size shouldBe 1
        graph.member.nameExact("callback").size shouldBe 1
        graph.member.nameExact("value").size shouldBe 1
        graph.call.nameExact("consume").argument.size shouldBe 1
      } finally graph.close()
    }
    "include declarations reached through unevaluated type syntax and renamed aliases" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "declaration-types-")
      Files.writeString(root.resolve("types.h"), "struct Item { long size; }; typedef struct Item First; typedef First Alias; int unused(void);")
      val source = root.resolve("sample.c")
      Files.writeString(source, "#include \"types.h\"\nunsigned long inspect(void){return sizeof(Alias);}")
      val library = Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY"))
      val document = ClangNative.load(library).parse(Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), root)
      val declarations = document("ast")("inner").arr
      declarations.exists(node => node.obj.get("name").exists(_.str == "Item")) shouldBe true
      declarations.exists(node => node.obj.get("name").exists(_.str == "unused")) shouldBe false
    }
  }
}
