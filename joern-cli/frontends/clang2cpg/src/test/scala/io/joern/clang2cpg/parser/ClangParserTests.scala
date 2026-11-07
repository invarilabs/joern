package io.joern.clang2cpg.parser

import io.joern.clang2cpg.{Config, Frontend}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scopt.OParser

class ClangParserTests extends AnyWordSpec with Matchers {
  "Clang parser selection" should {
    "select Clang by default as the replacement frontend" in {
      Config().parserBackend shouldBe "clang"
      OParser.parse(Frontend.cmdLineParser, Seq.empty[String], Config()).get.parserBackend shouldBe "clang"
    }
    "reject explicit CDT and removed completed-facts options" in {
      OParser.parse(Frontend.cmdLineParser, Seq("--parser", "cdt"), Config()).isDefined shouldBe false
      OParser.parse(
        Frontend.cmdLineParser,
        Seq("--parser", "clang", "--clang-facts-command", "/unused"),
        Config()
      ).isDefined shouldBe false
    }
    "accept the stock preprocessor-only mode" in {
      OParser.parse(Frontend.cmdLineParser, Seq("--print-ifdef-only"), Config()).get.printIfDefsOnly shouldBe true
    }
    "retain every stock C frontend option in explicit configuration" in {
      val arguments = Seq(
        "--include", "/headers/one", "--include", "/headers/two",
        "--define", "COUNT=2", "--define", "FEATURE",
        "--include-comments", "--log-problems", "--log-preprocessor",
        "--with-include-auto-discovery", "--skip-function-bodies", "--with-preprocessed-files",
        "--compilation-database", "/build/compile_commands.json", "--clang-compiler", "/toolchain/clang"
      )
      val selected = OParser.parse(Frontend.cmdLineParser, arguments, Config()).get
      selected.includePaths shouldBe Set("/headers/one", "/headers/two")
      selected.defines shouldBe Set("COUNT=2", "FEATURE")
      selected.includeComments shouldBe true
      selected.logProblems shouldBe true
      selected.logPreprocessor shouldBe true
      selected.includePathsAutoDiscovery shouldBe true
      selected.skipFunctionBodies shouldBe true
      selected.withPreprocessedFiles shouldBe true
      selected.compilationDatabaseFilename shouldBe Some("/build/compile_commands.json")
      selected.clangCompiler shouldBe Some("/toolchain/clang")
    }
    "honor an explicit discovery disable after an enable" in {
      val selected = OParser.parse(
        Frontend.cmdLineParser,
        Seq("--with-include-auto-discovery", "--no-include-auto-discovery"),
        Config()
      ).get
      selected.includePathsAutoDiscovery shouldBe false
    }
    "record the deprecated image-location compatibility flag explicitly" in {
      OParser.parse(Frontend.cmdLineParser, Seq("--no-image-locations"), Config()).get
        .legacyNoImageLocations shouldBe true
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
