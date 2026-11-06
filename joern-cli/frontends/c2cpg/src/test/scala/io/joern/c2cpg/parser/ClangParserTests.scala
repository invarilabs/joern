package io.joern.c2cpg.parser

import io.joern.c2cpg.{C2Cpg, Config, Frontend}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scopt.OParser

class ClangParserTests extends AnyWordSpec with Matchers {
  "Clang parser selection" should {
    "reject a preprocessing operation that would route a Clang request through CDT" in {
      OParser
        .parse(Frontend.cmdLineParser, Seq("--parser", "clang", "--print-ifdef-only"), Config())
        .isDefined shouldBe false
    }
    "reject the same incompatible operation through the programmatic API" in {
      val failure = intercept[IllegalArgumentException] {
        new C2Cpg().printIfDefsOnly(Config().copy(parserBackend = "clang", printIfDefsOnly = true))
      }
      failure.getMessage should include("Clang preprocessor-only mode is not implemented")
    }
    "accept an explicitly selected compiler and evidence configuration" in {
      val arguments = Seq(
        "--parser",
        "clang",
        "--clang-command",
        "/explicit/command.json",
        "--clang-plugin",
        "/explicit/collector.so",
        "--clang-evidence-dir",
        "/explicit/new-evidence"
      )
      OParser.parse(Frontend.cmdLineParser, arguments, Config()).isDefined shouldBe true
    }
    "reject the removed completed-facts command" in {
      OParser
        .parse(
          Frontend.cmdLineParser,
          Seq("--parser", "clang", "--clang-facts-command", "/obsolete/producer.json"),
          Config()
        )
        .isDefined shouldBe false
    }
  }
}
