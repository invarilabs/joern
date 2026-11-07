package io.joern.clang2cpg.parser

import io.joern.clang2cpg.{Clang2Cpg, Config}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import java.nio.file.{Files, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClangInterTuTests extends AnyWordSpec with Matchers {
  private def configuration(sources: Map[String,String]): Config = {
    val root=Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")),"clang-inter-tu-")
    sources.foreach {case(name,code) =>
      Files.createDirectories(root.resolve(name).getParent)
      Files.writeString(root.resolve(name),code)
    }
    val database=root.resolve("compile_commands.json")
    Files.writeString(database,ujson.write(sources.keys.toSeq.sorted.map(name => ujson.Obj(
      "directory" -> root.toString,"file" -> root.resolve(name).toString,
      "arguments" -> ujson.Arr(sys.env("CLANG_CPG_COMPILER"),"-std=gnu11","-c",root.resolve(name).toString)))))
    Config().withInputPath(root.toString).copy(compilationDatabaseFilename=Some(database.toString),
      clangPlugin=Some(sys.env("CLANG_CPG_PLUGIN")),clangEvidenceDir=Some(root.resolve("evidence").toString))
  }
  private def withGraph(sources: Map[String,String])(check: Cpg => Unit): Unit = {
    val graph=new Clang2Cpg().createCpg(configuration(sources)).get
    try {X2Cpg.applyDefaultOverlays(graph);check(graph)} finally graph.close()
  }
  "Compiler identity linking" should {
    "link external prototypes across TUs while preserving the source typedef contract" in {
      withGraph(Map("a.c" -> "typedef int Count; extern int target(Count); int run(Count n){return target(n);} int (*ref(void))(Count){return target;}",
        "b.c" -> "int target(int n){return n+1;}")) { graph =>
        val call=graph.call.nameExact("target").head
        call._callOut.collect {case m: io.shiftleft.codepropertygraph.generated.nodes.Method => m.filename}.toList shouldBe List("b.c")
        val original=call.tag.nameExact("clang.callCanonicalMethodFullName").value.head
        original should include ("Count")
        call.tag.nameExact("clang.callDeclaredFunctionType").value.size shouldBe 1
        val references=graph.methodRef.filter(_.code == "target").toList
        references.size shouldBe 1
        references.head.refOut.collect {case method: io.shiftleft.codepropertygraph.generated.nodes.Method => method.filename}.toList shouldBe List("b.c")
      }
    }
    "leave static definitions isolated and unresolved external boundaries explicit" in {
      withGraph(Map("left/common.c" -> "static int helper(int x){return x;} extern int missing(int); int a(int x){return helper(x)+missing(x);}",
        "right/common.c" -> "static int helper(int x){return x+1;} int b(int x){return helper(x);}")) { graph =>
        graph.call.nameExact("helper").toList.foreach(call => call._callOut.collect {case m: io.shiftleft.codepropertygraph.generated.nodes.Method => m.filename}.toList shouldBe List(call.method.filename))
        graph.call.nameExact("missing").flatMap(_._callOut).collect {case m: io.shiftleft.codepropertygraph.generated.nodes.Method => m.isExternal}.toList shouldBe List(true)
      }
    }
    "reject multiple external definitions rather than choosing one" in {
      val failure=new Clang2Cpg().createCpg(configuration(Map("a.c" -> "int duplicate(void){return 1;}","b.c" -> "int duplicate(void){return 2;}"))).failed.get
      Iterator.iterate(failure: Throwable)(_.getCause).takeWhile(_ != null).map(_.toString).mkString(" ") should include ("Ambiguous externally visible Clang definitions")
    }
  }
}
