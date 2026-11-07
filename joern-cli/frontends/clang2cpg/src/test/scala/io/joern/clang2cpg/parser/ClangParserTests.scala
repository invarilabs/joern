package io.joern.clang2cpg.parser

import io.joern.clang2cpg.{Clang2Cpg, Config, Frontend}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scopt.OParser

class ClangParserTests extends AnyWordSpec with Matchers {
  "Clang parser selection" should {
    "reject an implicit backend instead of replacing default CDT behavior" in {
      OParser.parse(Frontend.cmdLineParser, Seq.empty[String], Config()).isDefined shouldBe false
      new Clang2Cpg().createCpg(Config()).isFailure shouldBe true
    }
    "reject explicit CDT and removed completed-facts options" in {
      OParser.parse(Frontend.cmdLineParser, Seq("--parser", "cdt"), Config()).isDefined shouldBe false
      OParser.parse(
        Frontend.cmdLineParser,
        Seq("--parser", "clang", "--clang-facts-command", "/unused"),
        Config()
      ).isDefined shouldBe false
    }
    "reject a preprocessing operation that would route a Clang request through CDT" in {
      OParser
        .parse(Frontend.cmdLineParser, Seq("--parser", "clang", "--print-ifdef-only"), Config())
        .isDefined shouldBe false
    }
    "reject the same incompatible operation through the programmatic API" in {
      val failure = intercept[IllegalArgumentException] {
        new Clang2Cpg().printIfDefsOnly(Config().copy(parserBackend = "clang", printIfDefsOnly = true))
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
  }
}
