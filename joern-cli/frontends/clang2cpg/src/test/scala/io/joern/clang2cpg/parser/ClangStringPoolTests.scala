package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.clang.ClangStringPool
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClangStringPoolTests extends AnyWordSpec with Matchers {
  "Graph-scoped Clang strings" should {
    "share equal contents without changing any characters" in {
      val pool = new ClangStringPool
      val first = new String("{\"kind\":\"Int\",\"name\":\"\\u03bb\"}".toCharArray)
      val second = new String(first.toCharArray)
      (first eq second) shouldBe false
      val canonical = pool(first)
      (canonical eq first) shouldBe true
      (pool(second) eq canonical) shouldBe true
      canonical shouldBe second
    }
    "retain distinct contents and empty strings exactly" in {
      val pool = new ClangStringPool
      val lower = new String("value".toCharArray)
      val upper = new String("Value".toCharArray)
      (pool(lower) eq lower) shouldBe true
      (pool(upper) eq upper) shouldBe true
      pool("") shouldBe ""
      pool("value\u0000suffix") shouldBe "value\u0000suffix"
    }
    "avoid sharing canonical references across graph pools" in {
      val first = new String("graph-local".toCharArray)
      val second = new String("graph-local".toCharArray)
      val one = new ClangStringPool
      val two = new ClangStringPool
      val left = one(first)
      val right = two(second)
      (left eq right) shouldBe false
      (left eq first) shouldBe true
      (right eq second) shouldBe true
    }
    "reject null strings explicitly" in {
      val pool = new ClangStringPool
      intercept[IllegalArgumentException] { pool(null) }.getMessage should include("null")
    }
  }
}
