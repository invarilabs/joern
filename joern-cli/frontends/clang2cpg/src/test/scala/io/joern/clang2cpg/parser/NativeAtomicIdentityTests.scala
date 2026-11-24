package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.clang.{ClangAstEmitter, AtomicOperandRoots}
import io.joern.clang2cpg.parser.memory.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import java.nio.file.{Files, Path, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.collection.mutable

class NativeAtomicIdentityTests extends AnyWordSpec with Matchers {
  private val library = Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY"))
  private val compiler = sys.env("CLANG_CPG_COMPILER")
  private val source = "void f(int *p,int v,int order){__atomic_store_n(p,v,order);}"
  private def input(code: String): ClangInput = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "atomic-identity-")
    val file = directory.resolve("sample.c")
    Files.writeString(file, code)
    ClangInput(file, Seq(compiler, "-std=gnu11", file.toString), directory)
  }
  private def tags(node: StoredNode, name: String): List[String] =
    node._taggedByOut.collect { case entry: Tag if entry.name == name => entry.value }.toList
  private def tag(node: StoredNode, name: String): String = {
    val values = tags(node, name)
    values.size shouldBe 1
    values.head
  }
  private def syntax(node: NativeValue): Seq[NativeValue] =
    Seq(node) ++ node.obj.get("inner").toSeq.flatMap(_.arr).flatMap(syntax)
  private def emit(selected: ClangInput, document: NativeValue, graph: Cpg = Cpg.empty): Cpg = {
    val paths = Seq(library, Path.of(compiler), selected.source)
    val tools = paths.map(p => p.toRealPath().toString -> NativeGraphEvidence.digest(p.toRealPath())).toMap
    val aliases = paths.map(p => p.toString -> p.toRealPath().toString).toMap
    try {
      new ClangAstEmitter(graph, NativeGraphEvidence(selected.source, document, selected.command, tools, aliases),
        embedNativeEvidence = false).createAndApply()
      graph
    } catch { case error: Throwable => graph.close(); throw error }
  }
  private def withGraph(code: String)(check: Cpg => Unit): Unit = {
    val selected = input(code)
    val graph = NativeCpg.create(Seq(selected), selected.directory, selected.directory.resolve("graph.bin"),
      library, mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = false).get
    try check(graph)
    finally graph.close()
  }
  private def checkAtomic(graph: Cpg): Unit = {
    val calls = graph.call.filter(c => tags(c, "clang.atomic").nonEmpty).toList
    calls.nonEmpty shouldBe true
    calls.foreach { call =>
      val owner = tag(call, "clang.atomicExpression")
      owner.nonEmpty shouldBe true
      val unit = tag(call, "clang.translationUnitSource")
      val operation = ujson.read(tag(call, "clang.atomic"))
      val witnesses = operation("operands").arr.toList
      val args = call.argument.toList.sortBy(_.argumentIndex)
      args.size shouldBe witnesses.size
      args.map(_.id).distinct.size shouldBe args.size
      args.zip(witnesses).foreach { case (arg, witness) =>
        val relation = ujson.read(tag(arg, "clang.atomicOperand"))
        relation.obj.keySet shouldBe Set("operation", "role", "expression")
        relation("operation").str shouldBe owner
        relation("role").str shouldBe witness("role").str
        relation("expression").str shouldBe witness("expression").str
        arg._astIn.toList.map(_.id) shouldBe List(call.id)
        tag(arg, "clang.translationUnitSource") shouldBe unit
      }
      witnesses.map(_("expression").str).distinct.size shouldBe witnesses.size
    }
  }
  "JNI atomic operand identity" should {
    "bind exact original operand witnesses on direct children after conversion collapse" in {
      withGraph(source) { graph =>
        checkAtomic(graph)
        val store = graph.call.nameExact("__atomic_store_n").head
        val value = store.argument(2)
        value.isInstanceOf[Identifier] shouldBe true
        tags(value, "clang.implicitConversion").nonEmpty shouldBe true
        value.asInstanceOf[Identifier].refOut.size shouldBe 1
        graph.call.nameExact("ImplicitCastExpr").size shouldBe 0
      }
    }
    "retain init, narrowing, explicit casts, parentheses, equal-looking roles and macro ownership" in {
      withGraph("enum E{ORDER=5};\n#define STORE(p,v,o) __atomic_store_n(p,v,o)\n" +
        "void f(int *p,_Bool *b,int v){__atomic_store_n(p,5,5);" +
        "__atomic_store_n(p,(v),ORDER);__atomic_store_n(p,(int)v,5);" +
        "__atomic_store_n(b,v,5);__c11_atomic_init((_Atomic(int)*)p,v);STORE(p,v,5);}") { graph =>
        checkAtomic(graph)
        val equal = graph.call.nameExact("__atomic_store_n").find(_.argument.code.toList == List("p", "5", "5")).get
        val relations = equal.argument.toList.sortBy(_.argumentIndex).map(a => ujson.read(tag(a, "clang.atomicOperand")))
        relations(1)("expression").str should not be relations(2)("expression").str
        graph.call.nameExact("STORE").foreach(c => tags(c, "clang.atomicExpression") shouldBe Nil)
        graph.call.nameExact("<operator>.cast").nonEmpty shouldBe true
      }
    }
    "scope operation joins to their translation unit and leave ordinary calls untagged" in {
      for (code <- Seq(source, source.replace("void f", "void g"))) withGraph(code)(checkAtomic)
      withGraph("void ordinary(int *p,int v,int order){} void f(int *p){ordinary(p,5,5);}") { graph =>
        graph.call.nameExact("ordinary").foreach { c =>
          tags(c, "clang.atomicExpression") shouldBe Nil
          c.argument.foreach(a => tags(a, "clang.atomicOperand") shouldBe Nil)
        }
      }
    }
    "keep colliding compiler identities scoped to two TUs in one CPG" in {
      val inputs = Seq(input(source), input(source.replace("void f", "void g")))
      val documents = inputs.map(i => ClangNative.load(library).parse(i.command, i.directory))
      def atomic(document: NativeValue): NativeValue =
        syntax(document("ast")).find(n => n.obj.get("kind").exists(_.str == "AtomicExpr")).get
      val owners = documents.map(atomic)
      val witnesses = documents.zip(owners).map { case (d, n) => d("nodes")(n("id").str)("atomic") }
      val replacements = (Seq(owners(1)("id").str -> owners.head("id").str) ++
        witnesses(1)("operands").arr.zip(witnesses.head("operands").arr).map { case (a, b) =>
          a("expression").str -> b("expression").str
        }).toMap
      // Consistently rename only native expression IDs, retaining both real source closures.
      def collide(value: NativeValue): NativeValue = {
        if (value.strOpt.nonEmpty) NStr(replacements.getOrElse(value.str, value.str))
        else if (value.objOpt.nonEmpty) NObj.from(value.obj.toSeq.map { case (key, entry) =>
          replacements.getOrElse(key, key) -> collide(entry)
        })
        else if (value.arrOpt.nonEmpty) NArr.from(value.arr.map(collide))
        else value
      }
      val graph = Cpg.empty
      try {
        emit(inputs.head, documents.head, graph)
        emit(inputs(1), collide(documents(1)), graph)
        checkAtomic(graph)
        val calls = graph.call.nameExact("__atomic_store_n").toList
        calls.size shouldBe 2
        calls.map(c => tag(c, "clang.atomicExpression")).distinct.size shouldBe 1
        calls.map(c => tag(c, "clang.translationUnitSource")).toSet shouldBe inputs.map(_.source.toString).toSet
        def tuple(arg: Expression): (String, String, String, String) = {
          val relation = ujson.read(tag(arg, "clang.atomicOperand"))
          (tag(arg, "clang.translationUnitSource"), relation("operation").str,
            relation("role").str, relation("expression").str)
        }
        val left = calls.head.argument.toList.sortBy(_.argumentIndex).map(a => tuple(a))
        val right = calls(1).argument.toList.sortBy(_.argumentIndex).map(a => tuple(a))
        left.map(t => (t._2, t._3, t._4)) shouldBe right.map(t => (t._2, t._3, t._4))
        left.toSet.intersect(right.toSet) shouldBe empty
        calls.head.argument.foreach { foreign =>
          tag(foreign, "clang.translationUnitSource") should not be tag(calls(1), "clang.translationUnitSource")
          foreign._astIn.toList.map(_.id) shouldBe List(calls.head.id)
        }
      } finally graph.close()
    }
    "reject the same returned NewNode root with the actual production guard" in {
      val guard = new AtomicOperandRoots(Seq.empty)
      val first = NewIdentifier().name("v").code("v").typeFullName("int")
      val equalLooking = NewIdentifier().name("v").code("v").typeFullName("int")
      guard.claim(first)
      guard.claim(equalLooking)
      val error = intercept[IllegalArgumentException] { guard.claim(first) }
      error.getMessage should include ("Clang atomic operands share an emitted root")
    }
    "reject an already AST-owned returned NewNode with the actual production guard" in {
      val child = NewIdentifier().name("v").code("v").typeFullName("int")
      val parent = NewCall().name("prior").code("prior(v)")
      val parents = mutable.Map[NewNode, NewNode](child -> parent)
      val guard = new AtomicOperandRoots(parents.keys)
      val error = intercept[IllegalArgumentException] { guard.claim(child) }
      error.getMessage should include ("Clang atomic operand root already has an AST owner")
    }
    "reject incomplete, duplicate and unmatched native operand certificates" in {
      val mutations: Seq[(String, (NativeValue, NativeValue) => Unit)] = Seq(
        "missing expression" -> ((_, atomic) => atomic("operands").arr.head.obj.remove("expression")),
        "empty expression" -> ((_, atomic) => atomic("operands").arr.head.obj("expression") = NStr("")),
        "duplicate role" -> ((_, atomic) => atomic("operands").arr(1).obj("role") = atomic("operands").arr.head("role")),
        "duplicate expression" -> ((_, atomic) => atomic("operands").arr(1).obj("expression") = atomic("operands").arr.head("expression")),
        "unmatched expression" -> ((_, atomic) => atomic("operands").arr.head.obj("expression") = NStr("missing")),
        "incomplete coverage" -> ((_, atomic) => atomic("operands").arr.remove(0)),
        "duplicate candidate" -> ((node, _) => node("inner").arr += node("inner").arr.head)
      )
      mutations.foreach { case (name, mutate) =>
        withClue(name) {
          val selected = input(source)
          val document = ClangNative.load(library).parse(selected.command, selected.directory)
          val node = syntax(document("ast")).find(n => n.obj.get("kind").exists(_.str == "AtomicExpr")).get
          val atomic = document("nodes")(node("id").str)("atomic")
          mutate(node, atomic)
          intercept[IllegalArgumentException] { val graph = emit(selected, document); graph.close() }
        }
      }
    }
  }
}
