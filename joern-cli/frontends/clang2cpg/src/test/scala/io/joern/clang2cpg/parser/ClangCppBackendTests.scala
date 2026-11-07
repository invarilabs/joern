package io.joern.clang2cpg.parser

import io.joern.clang2cpg.{Clang2Cpg, Config}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes}
import io.shiftleft.semanticcpg.language.*
import java.nio.file.{Files, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClangCppBackendTests extends AnyWordSpec with Matchers {
  private def withGraph(code: String)(check: Cpg => Unit): Unit = {
    val root=Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")),"clang-cpp-")
    val source=root.resolve("sample.cpp")
    Files.writeString(source,code)
    val command=root.resolve("command.json")
    Files.writeString(command,ujson.write(Seq(sys.env("CLANG_CPG_COMPILER"),"-std=c++17",source.toString)))
    val config=Config(clangCommand=Some(command.toString),clangPlugin=Some(sys.env("CLANG_CPG_PLUGIN")),
      clangEvidenceDir=Some(root.resolve("evidence").toString)).withInputPath(source.toString)
    val graph=new Clang2Cpg().createCpg(config).get
    try {X2Cpg.applyDefaultOverlays(graph);check(graph)} finally graph.close()
  }
  "Native C++ class CPG" should {
    "retain instance fields, canonical method ownership, typed this and member call operands" in {
      withGraph("struct Counter {int value; int add(int n) {return value+n;}}; int run(Counter *c,int n){return c->add(n);}") { graph =>
        val record=graph.typeDecl.nameExact("Counter").head
        record.member.name.toList should contain ("value")
        val method=graph.method.nameExact("add").head
        method.astParent.id shouldBe record.id
        method.parameter.index(0).name.toList shouldBe List("this")
        method.parameter.index(0).typeFullName.head should include ("Counter")
        graph.identifier.nameExact("this").refsTo.name.toList should contain ("this")
        val call=graph.call.nameExact("add").head
        call.argument.argumentIndex(0).code.toList shouldBe List("c")
        call.argument.argumentIndex(1).code.toList shouldBe List("n")
        call._callOut.collect {case method: io.shiftleft.codepropertygraph.generated.nodes.Method => method.name}.toList shouldBe List("add")
      }
    }
    "retain const receivers, static calls and declaration-distinct overloads" in {
      withGraph("struct Box {int get() const {return 1;} int get(int n){return n;} static int twice(int n){return n+n;}}; int run(const Box *b){return b->get()+Box::twice(2);}") { graph =>
        graph.method.nameExact("get").fullName.toList.distinct.size shouldBe 2
        graph.method.nameExact("twice").parameter.index(0).size shouldBe 0
        graph.call.nameExact("twice").dispatchType.toList shouldBe List(DispatchTypes.STATIC_DISPATCH)
        graph.method.nameExact("get").parameter.index(0).typeFullName.toList.exists(_.contains("const")) shouldBe true
      }
    }
  }
}
