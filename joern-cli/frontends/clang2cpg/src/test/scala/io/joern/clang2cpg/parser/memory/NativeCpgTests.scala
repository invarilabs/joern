package io.joern.clang2cpg.parser.memory

import io.joern.clang2cpg.parser.ClangInput
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}
import scala.collection.mutable

class NativeCpgTests extends AnyWordSpec with Matchers {
  "Direct native Clang CPG" should {
    "build typed source expressions without AST dumps, evidence directories, or CDT" in {
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "jni-cpg-")
      val source = directory.resolve("sample.c")
      Files.writeString(source, "int increment(int value) {return value+1;}\n")
      val command = Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString)
      val timings = mutable.ArrayBuffer.empty[NativeCpg.Timing]
      val graph = NativeCpg.create(Seq(ClangInput(source, command, directory)), directory,
        directory.resolve("source.cpg.bin"), Paths.get(sys.env("CLANG_CPG_JNI")), timings, repository = false).get
      try {
        val method = graph.method.nameExact("increment").head
        method.parameter.name.toList shouldBe List("value")
        method.parameter.code.toList shouldBe List("int value")
        val additions = method.ast.isCall.nameExact("<operator>.addition").toList
        additions.size shouldBe 1
        additions.head.typeFullName shouldBe "int"
        method.ast.isIdentifier.nameExact("value").typeFullName.toList shouldBe List("int")
        graph.tag.nameExact("clang.nativeEvidence").size shouldBe 0
        val contract = ujson.read(graph.tag.nameExact("clang.translationUnit").value.head)
        contract("tools").obj should contain key source.toString
        timings.size shouldBe 1
        val files = Files.list(directory)
        try files.toArray.map(_.toString).exists(name => name.endsWith(".json") || name.endsWith(".ast") || name.endsWith(".zip")) shouldBe false
        finally files.close()
      } finally graph.close()
    }
    "reject invalid C and never publish an empty successful graph" in {
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "jni-invalid-")
      val source = directory.resolve("broken.c")
      Files.writeString(source, "int broken( {not C;}")
      val native = ClangNative.load(Paths.get(sys.env("CLANG_CPG_JNI")))
      intercept[IllegalArgumentException] {
        native.parse(Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), directory)
      }
    }
  }
}
