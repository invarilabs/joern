package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangCppNativeTests extends AnyWordSpec with Matchers {
  private def capture(code: String): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-cpp-native-")
    val source = directory.resolve("sample.cpp")
    Files.writeString(source, code)
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(compiler, "-std=c++17", source.toString)))
    ClangParser.capture(Config(clangCommand = Some(command.toString), clangPlugin = Some(plugin),
      clangEvidenceDir = Some(directory.resolve("evidence").toString))
      .withInputPath(source.toString)).document
  }

  "Native C++ facts" should {
    "reject unresolved dependent types instead of emitting an approximate type" in {
      val failure = intercept[IllegalArgumentException] {
        capture("template<class T> T identity(T value){return value;}")
      }
      failure.getMessage should include("Clang frontend failed")
    }
    "retain reference types, member owner identities and direct method targets" in {
      val document = capture("class A {int x; public:int f(int v){return x+v;}}; int read(A &a){return a.f(1);}")
      val nodes = document("nodes").obj.values.toSeq
      val parameter = nodes.find(node => node.obj.get("name").exists(_.str == "a")).get
      parameter("semantic_type")("kind").str shouldBe "LValueReference"
      parameter("semantic_type")("pointee")("kind").str shouldBe "Record"
      val method = nodes.find(node => node.obj.get("name").exists(_.str == "f")).get
      method("owner_usr").str should not be empty
      method("static_method").bool shouldBe false
      nodes.exists(node => node.obj.get("direct_callee_usr").contains(method("usr"))) shouldBe true
    }
    "retain member pointer, rvalue-reference and constructor identities" in {
      val document = capture("struct A {int x; A(int v):x(v){}}; int A::*member=&A::x; int read(A&& a){A b(2); return b.x;}")
      val nodes = document("nodes").obj.values.toSeq
      val member = nodes.find(node => node.obj.get("name").exists(_.str == "member")).get
      member("semantic_type")("kind").str shouldBe "MemberPointer"
      member("semantic_type")("owner")("kind").str shouldBe "Record"
      nodes.exists(node => node.obj.get("semantic_type").exists(value => value("kind").str == "RValueReference")) shouldBe true
      nodes.exists(_.obj.contains("constructor_usr")) shouldBe true
    }
  }
}
