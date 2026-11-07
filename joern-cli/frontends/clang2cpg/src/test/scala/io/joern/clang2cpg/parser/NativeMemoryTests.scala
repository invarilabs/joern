package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.memory.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import java.util.{LinkedHashMap, ArrayList}

class NativeMemoryTests extends AnyWordSpec with Matchers {
  "Typed native values" should {
    "preserve ordered objects, arrays and primitive types without parsing JSON" in {
      val raw = new LinkedHashMap[String, Object]()
      val elements = new ArrayList[Object]()
      elements.add("value")
      elements.add(java.lang.Long.valueOf(7))
      raw.put("items", elements)
      raw.put("typed", java.lang.Boolean.TRUE)
      val value = NativeValue.fromJava(raw)
      value.obj.keys.toList shouldBe List("items", "typed")
      value("items")(0).str shouldBe "value"
      value("items")(1).num shouldBe 7
      value("typed").bool shouldBe true
      (value.obj eq value.obj) shouldBe true
      (value("items").arr eq value("items").arr) shouldBe true
      value shouldBe NObj("items" -> NArr("value", 7), "typed" -> true)
      value.hashCode shouldBe NObj("items" -> NArr("value", 7), "typed" -> true).hashCode
      value.compact shouldBe "{\"items\":[\"value\",7],\"typed\":true}"
    }

    "fail on missing fields, wrong kinds, nulls and unsupported native objects" in {
      intercept[IllegalArgumentException] { NObj()("missing") }
      intercept[IllegalArgumentException] { NStr("value").num }
      intercept[IllegalArgumentException] { NNum(1).bool }
      intercept[IllegalArgumentException] { NArr()(0) }
      intercept[IllegalArgumentException] { NativeValue.fromJava(null) }
      intercept[IllegalArgumentException] { NativeValue.fromJava(new Object()) }
      intercept[IllegalArgumentException] { NNum(Double.NaN) }
      intercept[IllegalArgumentException] { NativeValue.fromJava(java.lang.Double.valueOf(Double.PositiveInfinity)) }
      intercept[IllegalArgumentException] { NObj("duplicate" -> 1, "duplicate" -> 2) }
      val invalid = new LinkedHashMap[String, Object]()
      invalid.put("missing", null)
      intercept[IllegalArgumentException] { NativeValue.fromJava(invalid).obj }
    }
  }

  "In-process native Clang" should {
    "use compiler installation paths for resource-header declarations" in {
      val native = ClangNative.load(Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")))
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "jni-resources-")
      val source = directory.resolve("resources.c")
      Files.writeString(source, "#include <stdatomic.h>\nint value(void){return 1;}\n")
      val document = native.parse(Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), directory)
      val physicalFiles = document("inputs").obj.keySet
      document("nodes").obj.values.flatMap(_.obj.get("file")).map(_.str).filter(name => Files.isRegularFile(Paths.get(name))).foreach { name =>
        physicalFiles should contain(name)
      }
    }
    "reject valid unsupported syntax instead of dropping its behavior" in {
      val native = ClangNative.load(Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")))
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "jni-unsupported-")
      val source = directory.resolve("loop.c")
      Files.writeString(source, "int count(void){int value=0;for(int i=0;i<3;i++){value+=i;}return value;}\n")
      val failure = intercept[IllegalArgumentException] {
        native.parse(Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), directory)
      }
      failure.getMessage should include("unsupported executable compiler syntax ForStmt")
    }
    "fail when the explicitly selected library is missing" in {
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-jni-missing-")
      intercept[IllegalArgumentException] { ClangNative.load(directory.resolve("missing.so")) }
    }

    "reject empty commands and invalid compiler input instead of selecting another parser" in {
      val library = Paths.get(sys.env.getOrElse("CLANG_CPG_JNI_LIBRARY",
        fail("Set CLANG_CPG_JNI_LIBRARY to the built direct Clang library")))
      val native = ClangNative.load(library)
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-jni-invalid-")
      intercept[IllegalArgumentException] { native.parse(Seq.empty, directory) }
      val source = directory.resolve("invalid.c")
      Files.writeString(source, "int broken( {\n")
      intercept[RuntimeException] { native.parse(Seq("clang", "-std=gnu11", source.toString), directory) }
    }

    "supply typed compiler expression facts without an external process or evidence files" in {
      val library = Paths.get(sys.env.getOrElse("CLANG_CPG_JNI_LIBRARY",
        fail("Set CLANG_CPG_JNI_LIBRARY to the built direct Clang library")))
      val native = ClangNative.load(library)
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-jni-expression-")
      val source = directory.resolve("sample.c")
      Files.writeString(source, "int increment(int value) { return value + 1; }\n")
      val document = native.parse(Seq("clang", "-std=gnu11", source.toString), directory)
      document("ast")("kind").str shouldBe "TranslationUnitDecl"
      document("inputs").obj should contain key source.toString
      val typed = document("nodes").obj.values.filter(_.obj.contains("semantic_type")).toList
      typed should not be empty
      typed.exists(_("semantic_type")("kind").str == "Int") shouldBe true
      val files = Files.list(directory)
      try {
        import scala.jdk.CollectionConverters.*
        files.iterator().asScala.map(_.getFileName.toString).toSet shouldBe Set("sample.c")
      } finally files.close()
    }
  }
}
