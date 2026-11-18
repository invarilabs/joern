package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangValueShapeNativeTests extends AnyWordSpec with Matchers {
  private def capture(code: String): io.joern.clang2cpg.parser.memory.NativeValue = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-value-shape-native-")
    val source = directory.resolve("sample.c")
    Files.writeString(source, code)
    val native = io.joern.clang2cpg.parser.memory.ClangNative.load(Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")))
    native.parse(Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), directory)
  }

  private def facts(doc: io.joern.clang2cpg.parser.memory.NativeValue): Seq[io.joern.clang2cpg.parser.memory.NativeValue] = {
    import io.joern.clang2cpg.parser.memory.*
    def walk(n: NativeValue): Seq[NativeValue] = Seq(n) ++ n.obj.get("inner").toSeq.flatMap(_.arr).flatMap(walk)
    walk(doc("ast")).filter(n => n.obj.contains("id") && doc("nodes").obj.contains(n("id").str)).map { n =>
      NObj((doc("nodes")(n("id").str).obj.toSeq ++ Seq("kind" -> n("kind")))*)
    }
  }

  "Native value shape evidence" should {
    "retain declaration-owned evaluated bounds and original parameter array types" in {
      val doc=capture("int measure(void); void f(int n, int p[n++]) { int a[n++]; int b[measure()][n++]; }\n")
      val nodes=facts(doc)
      val declarations=nodes.filter(n => n.obj.get("kind").exists(v => Set("VarDecl","ParmVarDecl")(v.str)))
      val bounded=declarations.filter(_.obj.contains("evaluated_bounds"))
      bounded.size shouldBe 3
      bounded.foreach(n => n("evaluated_bounds").arr should not be empty)
      val param=declarations.find(_.obj.get("name").exists(_.str=="p")).get
      param("original_semantic_type")("kind").str shouldBe "VariableArray"
    }
    "retain typedef-owned bounds without reevaluating array aliases" in {
      val doc=capture("void f(int n) { typedef int V[n++]; V a; }\n")
      val nodes=facts(doc)
      val alias=nodes.find(n => n.obj.get("kind").exists(_.str=="TypedefDecl") && n.obj.get("name").exists(_.str=="V")).get
      alias("evaluated_bounds").arr.size shouldBe 1
      val array=nodes.find(n => n.obj.get("kind").exists(_.str=="VarDecl") && n.obj.get("name").exists(_.str=="a")).get
      array("evaluated_bounds").arr.size shouldBe 0
    }
    "retain declaration identity for anonymous union aggregate carriers" in {
      val doc=capture("struct R { union { struct { int x; }; long y; }; }; struct R r={.x=1};\n")
      val nodes=facts(doc)
      val unions=nodes.filter(_.obj.contains("zero_initialized_union_field"))
      unions should not be empty
      val fields=nodes.filter(_.obj.get("kind").exists(_.str=="FieldDecl"))
      unions.foreach { owner =>
        val selected=owner("zero_initialized_union_field").str
        selected shouldBe owner("semantic_type")("declaration_usr").str + "#field:0"
        fields.count(_.obj.get("usr").exists(_.str==selected)) shouldBe 1
      }
    }
    "retain original fixed incomplete and nested array parameter constructors" in {
      val doc=capture("void f(int fixed[3], int open[], int nested[2][4]);\n")
      val parameters=facts(doc).filter(_.obj.get("kind").exists(_.str=="ParmVarDecl"))
      val byName=parameters.map(p => p("name").str -> p).toMap
      byName("fixed")("original_semantic_type")("kind").str shouldBe "ConstantArray"
      byName("fixed")("original_semantic_type")("extent").num shouldBe 3
      byName("open")("original_semantic_type")("kind").str shouldBe "IncompleteArray"
      val nested=byName("nested")("original_semantic_type")
      nested("kind").str shouldBe "ConstantArray"
      nested("extent").num shouldBe 2
      nested("element")("kind").str shouldBe "ConstantArray"
      nested("element")("extent").num shouldBe 4
      parameters.foreach(_.obj.contains("evaluated_bounds") shouldBe false)
    }
    "retain selected canonical union field and declared zero initialization field" in {
      val doc=capture("union U {int first; double second;}; union U x={.second=2.0};\n")
      val nodes=facts(doc)
      val init=nodes.find(_.obj.get("kind").exists(_.str=="InitListExpr")).get
      init("initialized_union_field").str should not be empty
      val record=nodes.find(n => n.obj.get("kind").exists(_.str=="RecordDecl") && n.obj.get("name").exists(_.str=="U")).get
      record("zero_initialized_union_field").str should not be empty
      init("initialized_union_field").str should not be record("zero_initialized_union_field").str
    }
  }
}
