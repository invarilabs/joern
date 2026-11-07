package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Path}

class ClangInputsTests extends AnyWordSpec with Matchers {
  private val executable = java.nio.file.Paths.get(sys.props("java.home"), "bin", "java").toString
  private def directory(): Path = Files.createTempDirectory("clang-inputs-").toRealPath()
  private def source(root: Path, name: String): Path = {
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, "int value(void) {return 1;}\n")
    path.toRealPath()
  }
  private def config(root: Path): Config =
    Config(clangCompiler = Some(executable)).withInputPath(root.toString)

  "Clang translation-unit planning" should {
    "discover deterministic C, C++ and header inputs and apply explicit exclusions" in {
      val root = directory()
      source(root, "z.cpp")
      source(root, "a.c")
      source(root, "api.hpp")
      source(root, "ignore.txt")
      source(root, "excluded/no.c")
      val selected = ClangInputs.plan(config(root).withIgnoredFiles(Seq("excluded")))
      selected.map(_.source.getFileName.toString) shouldBe Seq("a.c", "api.hpp", "z.cpp")
      selected.foreach(input => input.command.last shouldBe input.source.toString)
    }
    "prefer preprocessed input only when explicitly enabled" in {
      val root = directory()
      source(root, "a.c")
      source(root, "a.i")
      ClangInputs.plan(config(root)).map(_.source.getFileName.toString) shouldBe Seq("a.c")
      ClangInputs.plan(config(root).copy(withPreprocessedFiles = true))
        .map(_.source.getFileName.toString) shouldBe Seq("a.i")
    }
    "retain an explicitly selected single-file compiler command byte for byte" in {
      val root = directory()
      val input = source(root, "a.c")
      val command = root.resolve("argv.json")
      val arguments = Seq(executable, "-std=gnu11", "-DVALUE=1", input.toString)
      Files.writeString(command, ujson.write(arguments))
      ClangInputs.plan(config(input).copy(clangCommand = Some(command.toString))).head.command shouldBe arguments
    }
    "resolve arguments database paths against each entry directory and remove build outputs" in {
      val root = directory()
      val input = source(root, "src/a.c")
      Files.createDirectories(root.resolve("headers"))
      val database = root.resolve("compile_commands.json")
      Files.writeString(database, ujson.write(Seq(ujson.Obj(
        "directory" -> root.toString, "file" -> "src/a.c",
        "arguments" -> ujson.Arr(executable, "-Iheaders", "-iquote", "headers", "-c", "src/a.c", "-o", "a.o")
      ))))
      val selected = ClangInputs.plan(config(input).copy(compilationDatabaseFilename = Some(database.toString))).head
      selected.directory shouldBe root
      selected.command shouldBe Seq(executable, "-I" + root.resolve("headers"), "-iquote",
        root.resolve("headers").toString, input.toString)
    }
    "parse quoted command-form entries without invoking a shell" in {
      val root = directory()
      val input = source(root, "a.c")
      val database = root.resolve("compile_commands.json")
      Files.writeString(database, ujson.write(Seq(ujson.Obj(
        "directory" -> root.toString, "file" -> "a.c",
        "command" -> s"'$executable' '-DNAME=two words' -c a.c -o 'object file.o'"
      ))))
      ClangInputs.plan(config(input).copy(compilationDatabaseFilename = Some(database.toString)))
        .head.command shouldBe Seq(executable, "-DNAME=two words", input.toString)
    }
    "reject duplicate and missing translation-unit assignments" in {
      val root = directory()
      val input = source(root, "a.c")
      val entry = ujson.Obj("directory" -> root.toString, "file" -> input.toString,
        "arguments" -> ujson.Arr(executable, "-c", input.toString))
      val database = root.resolve("compile_commands.json")
      Files.writeString(database, ujson.write(Seq(entry, entry)))
      intercept[IllegalArgumentException] {
        ClangInputs.plan(config(input).copy(compilationDatabaseFilename = Some(database.toString)))
      }.getMessage should include("ambiguous")
      Files.writeString(database, "[]")
      intercept[IllegalArgumentException] {
        ClangInputs.plan(config(input).copy(compilationDatabaseFilename = Some(database.toString)))
      }.getMessage should include("missing")
    }
    "select only database translation units under a directory without scanning unrelated sources" in {
      val root = directory()
      val input = source(root, "built.c")
      source(root, "not-in-this-build.c")
      source(root, "standalone.h")
      val outside = source(directory(), "outside.c")
      val database = root.resolve("compile_commands.json")
      Files.writeString(database, ujson.write(Seq(input, outside).map(path => ujson.Obj(
        "directory" -> path.getParent.toString, "file" -> path.toString,
        "arguments" -> ujson.Arr(executable, "-c", path.toString)
      ))))
      val selected = ClangInputs.plan(config(root).copy(compilationDatabaseFilename = Some(database.toString)))
      selected.map(_.source) shouldBe Seq(input)
    }
    "preserve database entries and explicitly selected files when a sibling preprocessed file exists" in {
      val root = directory()
      val input = source(root, "a.c")
      source(root, "a.i")
      val database = root.resolve("compile_commands.json")
      Files.writeString(database, ujson.write(Seq(ujson.Obj(
        "directory" -> root.toString, "file" -> input.toString,
        "arguments" -> ujson.Arr(executable, "-c", input.toString)
      ))))
      ClangInputs.plan(config(root).copy(withPreprocessedFiles = true,
        compilationDatabaseFilename = Some(database.toString))).map(_.source) shouldBe Seq(input)
      ClangInputs.plan(config(input).copy(withPreprocessedFiles = true)).map(_.source) shouldBe Seq(input)
    }
    "resolve raw programmatic context paths from the process cwd while retaining database-relative build paths" in {
      val root = directory()
      val input = source(root, "src/a.c")
      val context = directory()
      val processCwd = java.nio.file.Paths.get("").toAbsolutePath.normalize()
      def cliRelative(path: Path): String = processCwd.relativize(path).toString
      val include = Files.createDirectories(context.resolve("cli-headers"))
      val database = context.resolve("commands.json")
      Files.writeString(database, ujson.write(Seq(ujson.Obj(
        "directory" -> root.toString, "file" -> "src/a.c",
        "arguments" -> ujson.Arr(executable, "-Ibuild-headers", "-c", "src/a.c")
      ))))
      val selected = ClangInputs.plan(config(input).copy(
        compilationDatabaseFilename = Some(cliRelative(database)),
        clangCompiler = Some(cliRelative(java.nio.file.Paths.get(executable))),
        includePaths = Set(cliRelative(include))
      )).head
      selected.directory shouldBe root
      selected.command shouldBe Seq(executable, "-I" + root.resolve("build-headers"), "-I" + include, input.toString)
      ClangInputs.plan(config(input).copy(
        clangCompiler = Some(cliRelative(java.nio.file.Paths.get(executable))),
        includePaths = Set(cliRelative(include))
      )).head.command shouldBe Seq(executable, "-I" + include, input.toString)
      val command = context.resolve("argv.json")
      val argv = Seq(executable, "-std=gnu11", input.toString)
      Files.writeString(command, ujson.write(argv))
      ClangInputs.plan(config(input).copy(clangCommand = Some(cliRelative(command)))).head.command shouldBe argv
    }
    "reject shell operators and response-file cycles" in {
      intercept[IllegalArgumentException] { ClangInputs.commandWords("clang a.c; touch marker") }
      val root = directory()
      val input = source(root, "a.c")
      Files.writeString(root.resolve("args.rsp"), "@args.rsp")
      val database = root.resolve("compile_commands.json")
      Files.writeString(database, ujson.write(Seq(ujson.Obj(
        "directory" -> root.toString, "file" -> input.toString,
        "arguments" -> ujson.Arr(executable, "@args.rsp", "-c", input.toString)
      ))))
      intercept[IllegalArgumentException] {
        ClangInputs.plan(config(input).copy(compilationDatabaseFilename = Some(database.toString)))
      }.getMessage should include("cycle")
    }
  }
}
