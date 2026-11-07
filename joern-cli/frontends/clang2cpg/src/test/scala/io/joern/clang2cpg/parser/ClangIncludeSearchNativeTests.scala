package io.joern.clang2cpg.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

class ClangIncludeSearchNativeTests extends AnyWordSpec with Matchers {
  private def capture(expose: Boolean): (ujson.Value, Seq[(Path, String)]) = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-search-native-")
    val source = directory.resolve("sample.c")
    Files.writeString(source, "#include <stddef.h>\nint read(void){return sizeof(size_t);}\n")
    val paths = Seq("quoted" -> "quoted", "angle-one" -> "angled", "angle-two" -> "angled", "system" -> "system")
      .map { case (name, group) => Files.createDirectory(directory.resolve(name)) -> group }
    val compiler = sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER"))
    val plugin = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN"))
    val output = directory.resolve("clang.json")
    val arguments = Seq(compiler, "-std=gnu11", "-fsyntax-only", "-iquote", paths(0)._1.toString,
      "-I", paths(1)._1.toString, "-I", paths(2)._1.toString, "-isystem", paths(3)._1.toString,
      "-Xclang", "-load", "-Xclang", plugin, "-Xclang", "-add-plugin", "-Xclang", "clang-cpg") ++
      (Seq(output.toString, "direct-cpg-only") ++ Option.when(expose)("include-search-paths")).flatMap(value =>
        Seq("-Xclang", "-plugin-arg-clang-cpg", "-Xclang", value)) ++ Seq(source.toString)
    val process = new ProcessBuilder(arguments.asJava).directory(directory.toFile).redirectError(directory.resolve("compiler.log").toFile).start()
    process.waitFor() shouldBe 0
    ujson.read(Files.readString(output)) -> paths
  }

  "Compiler-owned include search evidence" should {
    "expose actual ordered quoted angled and system search entries when selected" in {
      val (document, requested) = capture(expose = true)
      val search = document("include_search")
      search("schema_version").num shouldBe 1
      search("compiler_owned").bool shouldBe true
      val workingDirectory = Paths.get(search("working_directory").str)
      workingDirectory.isAbsolute shouldBe true
      workingDirectory shouldBe requested.head._1.getParent.toRealPath()
      val entries = search("entries").arr
      entries.take(requested.size).map(entry => entry("path").str -> entry("group").str).toSeq shouldBe
        requested.map { case (path, group) => path.toString -> group }
      entries.size should be > requested.size
      entries.foreach { entry =>
        entry("path").str should not be empty
        Set("directory", "framework", "header_map") should contain(entry("kind").str)
        entry("characteristic").num should be >= 0.0
      }
    }
    "leave include search unreported unless explicitly selected" in {
      val (document, _) = capture(expose = false)
      document.obj should not contain key("include_search")
    }
  }
}
