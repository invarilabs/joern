package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

final case class ClangEvidence(source: Path, directory: Path, document: ujson.Value) {
  def validate(): Unit = {
    val inputs = document("inputs").obj
    require(inputs.nonEmpty, "Clang evidence lacks the source/header closure")
    inputs.foreach { case (name, expected) =>
      val path = if (Paths.get(name).isAbsolute) Paths.get(name) else source.getParent.resolve(name)
      require(
        Files.isRegularFile(path) && ClangParser.digest(path) == expected.str,
        s"Clang source/header evidence is stale or missing: $path; rebuild the graph"
      )
    }
  }
}

object ClangParser {
  def digest(path: Path): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val stream = Files.newInputStream(path)
    try {
      val buffer = new Array[Byte](65536)
      var count  = stream.read(buffer)
      while (count != -1) {
        digest.update(buffer, 0, count)
        count = stream.read(buffer)
      }
    } finally stream.close()
    digest.digest().map(value => f"${value & 0xff}%02x").mkString
  }

  /** Runs one explicitly configured native frontend. A compiler failure never selects CDT. */
  def capture(config: Config): ClangEvidence = {
    val source = Paths.get(config.inputPath).toRealPath()
    require(
      Files.isRegularFile(source) && source.toString.endsWith(".c"),
      "Clang prototype requires one explicit .c translation unit; directory mode is not implemented"
    )
    require(
      config.includePaths.isEmpty && config.defines.isEmpty && config.compilationDatabaseFilename.isEmpty &&
        !config.includePathsAutoDiscovery && !config.skipFunctionBodies && !config.withPreprocessedFiles &&
        !config.cTypeEvidence && !config.printIfDefsOnly && !config.includeComments,
      "Clang prototype takes its complete build configuration from --clang-command; conflicting options are unsupported"
    )
    def required(value: Option[String], option: String): Path =
      Paths
        .get(value.getOrElse(throw new IllegalArgumentException(s"Clang requires $option")))
        .toAbsolutePath
        .normalize()
    val commandFile = required(config.clangCommand, "--clang-command")
    val plugin      = required(config.clangPlugin, "--clang-plugin")
    val directory   = required(config.clangEvidenceDir, "--clang-evidence-dir")
    require(
      Files.isRegularFile(commandFile) && Files.isRegularFile(plugin),
      "Clang command JSON or matching exporter library is missing; configure explicit existing files"
    )
    require(!Files.exists(directory), s"Clang evidence path already exists: $directory; select a new directory")
    val selected = ujson.read(Files.readString(commandFile)).arr.toSeq.map(_.str)
    require(
      selected.size >= 2 && selected.last == source.toString,
      "Clang command must end with the exact selected absolute source path"
    )
    val compiler = Paths.get(selected.head)
    require(
      compiler.isAbsolute && Files.isRegularFile(compiler) && Files.isExecutable(compiler),
      "Clang command requires an explicit absolute compiler executable"
    )
    require(
      !selected.exists(value => Set("-o", "-c", "-S", "-emit-ast", "-fsyntax-only", "-Xclang")(value)),
      "Clang command must contain only the compiler, build flags and source; capture/output options are owned by the parser"
    )
    val protectedInputs = Seq(source, compiler, commandFile, plugin).map(path => path -> digest(path))
    Files.createDirectories(directory)
    val documentPath = directory.resolve("clang.json")
    val command      = selected.dropRight(1) ++ Seq(
      "-emit-ast",
      "-o",
      directory.resolve("compiler-types.ast").toString,
      "-Xclang",
      "-load",
      "-Xclang",
      plugin.toString,
      "-Xclang",
      "-add-plugin",
      "-Xclang",
      "clang-cpg",
      "-Xclang",
      "-plugin-arg-clang-cpg",
      "-Xclang",
      documentPath.toString,
      source.toString
    )
    Files.writeString(directory.resolve("command.json"), ujson.write(command, indent = 2))
    val started = System.nanoTime()
    val process = new ProcessBuilder(command.asJava)
      .directory(source.getParent.toFile)
      .redirectOutput(directory.resolve("compiler.stdout").toFile)
      .redirectError(directory.resolve("compiler.log").toFile)
      .start()
    if (!process.waitFor(300, TimeUnit.SECONDS)) {
      process.destroyForcibly().waitFor()
      throw new IllegalStateException(
        s"Clang timed out; inspect $directory and correct the compiler/build configuration"
      )
    }
    Files.writeString(
      directory.resolve("exit.json"),
      ujson.write(
        ujson.Obj("returncode" -> process.exitValue(), "seconds" -> (System.nanoTime() - started).toDouble / 1e9),
        indent = 2
      )
    )
    require(
      process.exitValue() == 0 && Files.isRegularFile(documentPath),
      s"Clang frontend failed or did not emit complete evidence; inspect ${directory.resolve("compiler.log")}"
    )
    protectedInputs.foreach { case (path, expected) =>
      require(digest(path) == expected, s"Clang input/tool changed during capture: $path; prepare fresh evidence")
    }
    val document = ujson.read(Files.readString(documentPath))
    require(
      document("schema_version").num == 1 && document("ast")("kind").str == "TranslationUnitDecl",
      "Clang exporter returned an unsupported or incomplete schema; use the matching exporter"
    )
    val artifacts = Seq(documentPath, directory.resolve("compiler-types.ast"), directory.resolve("clang.json.tokens"))
    artifacts.foreach(path =>
      require(
        Files.isRegularFile(path) && Files.size(path) > 0,
        s"Clang did not emit required native evidence: $path; rebuild with the matching exporter"
      )
    )
    val evidence = ClangEvidence(source, directory, document)
    evidence.validate()
    Files.writeString(
      directory.resolve("receipt.json"),
      ujson.write(
        ujson.Obj(
          "parser"           -> "clang",
          "source"           -> source.toString,
          "selected_command" -> ujson.Arr.from(selected),
          "artifacts" -> ujson.Obj.from(artifacts.map(path => path.getFileName.toString -> ujson.Str(digest(path)))),
          "command_sha256"  -> digest(directory.resolve("command.json")),
          "evidence_sha256" -> digest(documentPath),
          "tools" -> ujson.Obj.from(protectedInputs.map { case (path, hash) => path.toString -> ujson.Str(hash) })
        ),
        indent = 2
      )
    )
    evidence
  }
}
