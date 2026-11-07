package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.clang.ClangTagPool
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewLiteral, NewTag}
import io.shiftleft.semanticcpg.language.*
import flatgraph.DiffGraphApplier
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClangTagPoolTests extends AnyWordSpec with Matchers {
  "Graph-scoped Clang tags" should {
    "share one stored tag across two applied passes and deduplicate identical associations" in {
      val graph = Cpg.empty
      try {
        val pool = new ClangTagPool
        val first = NewLiteral().code("1")
        val one = Cpg.newDiffGraphBuilder
        one.addNode(first)
        val firstPass = pool.beginPass(graph)
        firstPass.attach(one, first, "clang.kind", "IntegerLiteral")
        firstPass.attach(one, first, "clang.kind", "IntegerLiteral")
        DiffGraphApplier.applyDiff(graph.graph, one)
        firstPass.publish()
        val firstId = first.storedRef.get.id
        val second = NewLiteral().code("2")
        val two = Cpg.newDiffGraphBuilder
        two.addNode(second)
        val secondPass = pool.beginPass(graph)
        secondPass.attach(two, second, "clang.kind", "IntegerLiteral")
        secondPass.attach(two, second, "clang.kind", "IntegerLiteral")
        DiffGraphApplier.applyDiff(graph.graph, two)
        secondPass.publish()
        graph.tag.size shouldBe 1
        graph.graph.edgeCount shouldBe 2
        graph.literal.map(_._taggedByOut.size).toList shouldBe List(1, 1)
        first.storedRef.get.id shouldBe firstId
      } finally graph.close()
    }
    "retain distinct tag names and values" in {
      val graph = Cpg.empty
      try {
        val pool = new ClangTagPool
        val node = NewLiteral().code("1")
        val diff = Cpg.newDiffGraphBuilder
        diff.addNode(node)
        val pass = pool.beginPass(graph)
        pass.attach(diff, node, "name", "one")
        pass.attach(diff, node, "name", "two")
        pass.attach(diff, node, "other", "one")
        DiffGraphApplier.applyDiff(graph.graph, diff)
        pass.publish()
        graph.tag.map(tag => tag.name -> tag.value).toSet shouldBe Set("name" -> "one", "name" -> "two", "other" -> "one")
      } finally graph.close()
    }
    "preserve exact source AST node IDs compared with separate tags" in {
      def build(shared: Boolean): List[(String, Long)] = {
        val graph = Cpg.empty
        try {
          val pool = new ClangTagPool
          Seq("1", "2").foreach { code =>
            val diff = Cpg.newDiffGraphBuilder
            val node = NewLiteral().code(code)
            diff.addNode(node)
            if (shared) {
              val pass = pool.beginPass(graph)
              pass.attach(diff, node, "kind", "literal")
              DiffGraphApplier.applyDiff(graph.graph, diff)
              pass.publish()
            } else {
              val tag = NewTag().name("kind").value("literal")
              diff.addNode(tag)
              diff.addEdge(node, tag, "TAGGED_BY")
              DiffGraphApplier.applyDiff(graph.graph, diff)
            }
          }
          graph.literal.map(node => node.code -> node.id).toList
        } finally graph.close()
      }
      build(shared = true) shouldBe build(shared = false)
    }
    "isolate graphs and reject reuse of a pool for another graph" in {
      val one = Cpg.empty
      val two = Cpg.empty
      try {
        val pool = new ClangTagPool
        val diff = Cpg.newDiffGraphBuilder
        val node = NewLiteral().code("1")
        diff.addNode(node)
        val pass = pool.beginPass(one)
        pass.attach(diff, node, "kind", "literal")
        DiffGraphApplier.applyDiff(one.graph, diff)
        pass.publish()
        intercept[IllegalArgumentException](pool.beginPass(two)).getMessage should include("different graph")
        val other = new ClangTagPool
        val otherDiff = Cpg.newDiffGraphBuilder
        val otherNode = NewLiteral().code("1")
        otherDiff.addNode(otherNode)
        val otherPass = other.beginPass(two)
        otherPass.attach(otherDiff, otherNode, "kind", "literal")
        DiffGraphApplier.applyDiff(two.graph, otherDiff)
        otherPass.publish()
        (one.tag.head.graph eq two.tag.head.graph) shouldBe false
      } finally {one.close(); two.close()}
    }
    "reject null metadata instead of creating an invalid tag" in {
      val graph = Cpg.empty
      try {
        val pool = new ClangTagPool
        val pass = pool.beginPass(graph)
        val diff = Cpg.newDiffGraphBuilder
        val node = NewLiteral().code("1")
        diff.addNode(node)
        intercept[IllegalArgumentException](pass.attach(diff, node, null, "value")).getMessage should include("null")
        intercept[IllegalArgumentException](pass.attach(diff, node, "name", null)).getMessage should include("null")
      } finally graph.close()
    }
    "reject unresolved pending tags and overlapping passes" in {
      val graph = Cpg.empty
      try {
        val pool = new ClangTagPool
        val pass = pool.beginPass(graph)
        val diff = Cpg.newDiffGraphBuilder
        val node = NewLiteral().code("1")
        diff.addNode(node)
        pass.attach(diff, node, "kind", "literal")
        intercept[IllegalArgumentException](pass.publish()).getMessage should include("unresolved")
        intercept[IllegalArgumentException](pool.beginPass(graph)).getMessage should include("unpublished")
      } finally graph.close()
    }
  }
}
