package io.joern.clang2cpg.parser.compat

import flatgraph.DiffGraphApplier
import io.joern.clang2cpg.Config
import io.joern.clang2cpg.parser.{ClangEvidence, ClangParser}
import io.joern.c2cpg.astcreation.AstCreator
import io.joern.c2cpg.parser.HeaderFileFinder
import io.joern.c2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.eclipse.cdt.internal.core.dom.parser.c.CASTTranslationUnit
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path, Paths}

/** Uses the stock AstCreator without invoking the CDT parser. */
object ClangCdtCompatibilityTests {
  def capture(code: String): ClangEvidence = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-cdt-compat-")
    val source = directory.resolve("sample.c")
    Files.writeString(source, code)
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString)))
    ClangParser.capture(
      Config(
        clangCommand = Some(command.toString),
        clangPlugin = Some(sys.env("CLANG_CPG_PLUGIN")),
        clangEvidenceDir = Some(directory.resolve("native").toString),
        embedNativeEvidence = false
      ).withInputPath(source.toString)
    )
  }

  def graph(evidence: ClangEvidence): Cpg = {
    val translationUnit = ClangCdtAst.translate(evidence)
    val config = io.joern.c2cpg.Config().withInputPath(evidence.source.getParent.toString)
    val accumulator = AstCreationPass.Accumulator()
    val diff = new AstCreator(
      evidence.source.getFileName.toString,
      accumulator,
      config,
      translationUnit,
      new HeaderFileFinder(config)
    )(config.schemaValidation).createAst()
    val result = Cpg.empty
    try {
      DiffGraphApplier.applyDiff(result.graph, diff)
      result
    } catch {
      case failure: Throwable =>
        result.close()
        throw failure
    }
  }
}

class ClangCdtCompatibilityTests extends AnyWordSpec with Matchers {
  "Clang-backed CDT compatibility" should {
    "translate compiler-captured arithmetic into the unchanged stock AstCreator" in {
      val evidence = ClangCdtCompatibilityTests.capture("int increment(int value) { return value + 1; }\n")
      ClangCdtAst.preflight(evidence)
      ClangCdtAst.translate(evidence) shouldBe a[CASTTranslationUnit]
      val graph = ClangCdtCompatibilityTests.graph(evidence)
      try {
        val method = graph.method.nameExact("increment").head
        method.parameter.name.toList shouldBe List("value")
        method.ast.isReturn.size shouldBe 1
        method.ast.isCall.nameExact("<operator>.addition").size shouldBe 1
        method.ast.isLiteral.code.toList shouldBe List("1")
        method.ast.isIdentifier.name.toList should contain("value")
      } finally graph.close()
    }

    "fail loudly for an unsupported CDT compatibility method" in {
      val evidence = ClangCdtCompatibilityTests.capture("int value(void) { return 1; }\n")
      val unit = ClangCdtAst.translate(evidence)
      val failure = intercept[LinkageError] { unit.getReferences(null) }
      failure.getMessage should include("getReferences")
    }

    "reject unsupported compiler atomic syntax before producing a partial translation unit" in {
      val evidence = ClangCdtCompatibilityTests.capture(
        "int increment(int *value) { return __atomic_fetch_add(value, 1, __ATOMIC_SEQ_CST); }\n"
      )
      val failure = intercept[LinkageError] { ClangCdtAst.preflight(evidence) }
      failure.getMessage should include("AtomicExpr")
      intercept[LinkageError] { ClangCdtAst.translate(evidence) }
    }
  }
}

/** Explicit fixture entrypoint; missing query_ctx evidence is an error, never a skipped test. */
object ClangCdtQueryCtxProof {
  def main(arguments: Array[String]): Unit = {
    require(arguments.length == 3, "Expected: native-clang.json source.c report.json")
    val capture = Paths.get(arguments(0)).toRealPath()
    val source = Paths.get(arguments(1)).toRealPath()
    val report = Paths.get(arguments(2)).toAbsolutePath.normalize()
    require(!Files.exists(report), "Compatibility proof report already exists: " + report)
    require(source.getFileName.toString == "query_ctx.c", "This proof requires the actual query_ctx.c source")
    val started = System.nanoTime()
    val document = {
      val input = Files.newInputStream(capture)
      try ujson.read(input) finally input.close()
    }
    val evidence = ClangEvidence(source, capture.getParent, document)
    evidence.validate()
    try {
      ClangCdtAst.preflight(evidence)
      val graph = ClangCdtCompatibilityTests.graph(evidence)
      try {
        require(graph.method.nonEmpty, "Compatibility translation produced no methods")
        Files.writeString(report, ujson.write(ujson.Obj(
          "status" -> "pass",
          "source" -> source.toString,
          "method_count" -> graph.method.size,
          "seconds" -> ((System.nanoTime() - started).toDouble / 1e9)
        ), indent = 2) + "\n")
      } finally graph.close()
    } catch {
      case failure: LinkageError =>
        Files.writeString(report, ujson.write(ujson.Obj(
          "status" -> "blocked",
          "source" -> source.toString,
          "blocker" -> failure.getMessage,
          "seconds" -> ((System.nanoTime() - started).toDouble / 1e9)
        ), indent = 2) + "\n")
        throw failure
    }
  }
}
