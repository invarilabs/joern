package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.memory.NativeCpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

class NativeConcurrencyTests extends AnyWordSpec with Matchers {
  private def inputs(): (Path, Seq[ClangInput]) = {
    val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "native-workers-")
    Files.writeString(root.resolve("common.h"), "struct Shared { int field; }; static int helper(int value){return value+1;}")
    val selected = Seq("alpha", "beta").map { name =>
      val source = root.resolve(name + ".c")
      Files.writeString(source, "#include \"common.h\"\nint " + name + "(struct Shared value){return helper(value.field);}")
      ClangInput(source, Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), root)
    }
    root -> selected
  }
  private def projection(graph: Cpg): List[String] = {
    val methods = graph.method.map(value => (value.name, value.fullName, value.signature).toString).toList
    val calls = graph.call.map(value => (value.name, value.code, value.typeFullName, value.methodFullName).toString).toList
    val references = graph.identifier.map(value =>
      (value.code, value.typeFullName, value._refOut.map(_.label).toList.sorted).toString).toList
    (methods ++ calls ++ references).sorted
  }
  "Bounded native capture" should {
    "retain the same graph and reference projection with one and two workers" in {
      val (root, selected) = inputs()
      val library = Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY"))
      val serial = NativeCpg.create(selected, root, root.resolve("serial.bin"), library,
        mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = true, workers = 1).get
      try {
        val parallel = NativeCpg.create(selected, root, root.resolve("parallel.bin"), library,
          mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = true, workers = 2).get
        try projection(parallel) shouldBe projection(serial) finally parallel.close()
      } finally serial.close()
    }
    "fail the complete build when a concurrent translation unit is invalid" in {
      val (root, selected) = inputs()
      Files.writeString(selected.last.source, "int invalid( {")
      NativeCpg.create(selected, root, root.resolve("invalid.bin"), Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")),
        mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = true, workers = 2).isFailure shouldBe true
    }
  }
}
