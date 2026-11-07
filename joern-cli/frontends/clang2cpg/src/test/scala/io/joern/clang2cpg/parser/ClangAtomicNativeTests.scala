package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangAtomicNativeTests extends AnyWordSpec with Matchers {
  private def capture(code: String): ujson.Value = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-atomic-native-")
    val source = directory.resolve("sample.c")
    Files.writeString(source, code)
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(compiler, "-std=gnu11", source.toString)))
    ClangParser.capture(Config(clangCommand = Some(command.toString), clangPlugin = Some(plugin),
      clangEvidenceDir = Some(directory.resolve("evidence").toString), embedNativeEvidence = false)
      .withInputPath(source.toString)).document
  }
  private def operands(document: ujson.Value, name: String): Seq[(String, String)] = {
    val nodes = document("nodes").obj
    val atomic = nodes.values.find(node => node.obj.get("atomic").exists(_("name").str == name)).get("atomic")
    atomic("op_code").num should be >= 0.0
    atomic("operands").arr.map(value => value("role").str -> nodes(value("expression").str)("code").str).toSeq
  }

  "Native atomic operation evidence" should {
    "restore source argument order rather than normalized AST order" in {
      val document = capture("int value(void); int order(void); int read(int *ptr){return __atomic_fetch_add(ptr,value(),order());}\n")
      operands(document, "__atomic_fetch_add") shouldBe Seq("pointer" -> "ptr", "value" -> "value()", "order" -> "order()")
    }
    "preserve GNU exchange and compare-exchange operands and controls" in {
      val document = capture("int read(int *ptr,int *expected,int *desired,int *result,int weak,int success,int failure){__atomic_exchange(ptr,desired,result,success);return __atomic_compare_exchange(ptr,expected,desired,weak,success,failure);}\n")
      operands(document, "__atomic_exchange").map(_._1) shouldBe Seq("pointer", "value", "result", "order")
      operands(document, "__atomic_compare_exchange") shouldBe Seq("pointer" -> "ptr", "expected" -> "expected",
        "desired" -> "desired", "weak" -> "weak", "order" -> "success", "failure_order" -> "failure")
    }
    "retain GNU load-copy, store, subtraction and value exchange variants" in {
      val document = capture("int read(int *ptr,int *expected,int value){int result;__atomic_load(ptr,&result,0);__atomic_store(ptr,&value,0);__atomic_fetch_sub(ptr,value,0);__atomic_exchange_n(ptr,value,0);return __atomic_compare_exchange_n(ptr,expected,value,0,0,0); }\n")
      operands(document, "__atomic_load").map(_._1) shouldBe Seq("pointer", "result", "order")
      Seq("__atomic_store", "__atomic_fetch_sub", "__atomic_exchange_n").foreach { name =>
        operands(document, name).map(_._1) shouldBe Seq("pointer", "value", "order")
      }
      operands(document, "__atomic_compare_exchange_n").map(_._1) shouldBe
        Seq("pointer", "expected", "desired", "weak", "order", "failure_order")
    }
    "preserve C11 init load store exchange and compare-exchange source roles" in {
      val document = capture("int read(_Atomic(int)*ptr,int *expected,int value,int success,int failure){__c11_atomic_init(ptr,value);__c11_atomic_store(ptr,value,success);__c11_atomic_exchange(ptr,value,success);__c11_atomic_compare_exchange_strong(ptr,expected,value,success,failure);return __c11_atomic_load(ptr,success);}\n")
      operands(document, "__c11_atomic_init").map(_._1) shouldBe Seq("pointer", "value")
      operands(document, "__c11_atomic_load").map(_._1) shouldBe Seq("pointer", "order")
      operands(document, "__c11_atomic_store").map(_._1) shouldBe Seq("pointer", "value", "order")
      operands(document, "__c11_atomic_exchange").map(_._1) shouldBe Seq("pointer", "value", "order")
      operands(document, "__c11_atomic_compare_exchange_strong").map(_._1) shouldBe
        Seq("pointer", "expected", "desired", "order", "failure_order")
    }
  }
}
