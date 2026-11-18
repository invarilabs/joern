package io.joern.clang2cpg.parser.memory

import io.joern.clang2cpg.parser.ClangInput
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.Block
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}
import scala.collection.mutable

class NativeEmptyBodyTests extends AnyWordSpec with Matchers {
  "Native explicit empty control bodies" should {
    "preserve NullStmt bodies for while do for and if without inventing execution" in {
      val directory=Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")),"jni-empty-bodies-")
      val source=directory.resolve("sample.c")
      Files.writeString(source,"int probe(void); void f(int n){; while(probe()); do;while(n--); for(;n;--n); if(n);else; while(n){n--;} }\n")
      val graph=NativeCpg.create(Seq(ClangInput(source,Seq(sys.env("CLANG_CPG_COMPILER"),"-std=gnu11",source.toString),directory)),directory,directory.resolve("source.cpg.bin"),Paths.get(sys.env("CLANG_CPG_JNI")),mutable.ArrayBuffer.empty[NativeCpg.Timing],repository=false).get
      try {
        graph.method.nameExact("f").head.ast.isBlock.size shouldBe 7
        val loops=graph.controlStructure.controlStructureTypeExact("WHILE").toList
        loops.size shouldBe 2
        val empty=loops.find(_.code.contains("probe")).get.out(EdgeTypes.TRUE_BODY).toList
        empty.size shouldBe 1
        empty.head.isInstanceOf[Block] shouldBe true
        empty.head.asInstanceOf[Block].astChildren.size shouldBe 0
        graph.controlStructure.controlStructureTypeExact("DO").head.out(EdgeTypes.DO_BODY).size shouldBe 1
        graph.controlStructure.controlStructureTypeExact("FOR").head.out(EdgeTypes.FOR_BODY).size shouldBe 1
        val branch=graph.controlStructure.controlStructureTypeExact("IF").head
        branch.out(EdgeTypes.TRUE_BODY).size shouldBe 1
        branch.out(EdgeTypes.FALSE_BODY).size shouldBe 1
        loops.find(loop => !loop.code.contains("probe")).get.out(EdgeTypes.TRUE_BODY).toList.head.asInstanceOf[Block].ast.isCall.nameExact("<operator>.postDecrement").size shouldBe 1
      } finally graph.close()
    }
  }
}
