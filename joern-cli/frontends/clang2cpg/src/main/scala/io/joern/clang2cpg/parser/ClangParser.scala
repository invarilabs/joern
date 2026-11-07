package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

final case class ClangEvidence(
  source: Path,
  directory: Path,
  document: ujson.Value
) {
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
  private[parser] final case class ProtectedInput(alias: Path, target: Path, sha256: String) {
    def validate(): Unit = {
      require(Files.isRegularFile(alias) && alias.toRealPath() == target,
        s"Clang input/tool alias target changed during capture: $alias; prepare fresh evidence")
      require(Files.isRegularFile(target) && digest(target) == sha256,
        s"Clang input/tool changed during capture: $target; prepare fresh evidence")
    }
  }

  private[parser] def protect(path: Path): ProtectedInput = {
    val alias = path.toAbsolutePath
    val target = alias.toRealPath()
    require(Files.isRegularFile(target), s"Clang protected input/tool is not a regular file: $path")
    ProtectedInput(alias, target, digest(target))
  }

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
    val inputs = ClangInputs.plan(config)
    require(inputs.size == 1, "capture(config) requires one translation unit; use the planned per-TU capture API for repositories")
    capture(inputs.head, ClangInputs.plugin(config), ClangInputs.evidenceDirectory(config), config)
  }

  def capture(input: ClangInput, plugin: Path, directory: Path, config: Config): ClangEvidence = {
    val source = input.source
    val selected = input.command
    require(!Files.exists(directory), s"Clang evidence path already exists: $directory; select a new directory")
    require(selected.size >= 2 && selected.last == source.toString, "Clang command must end with the exact selected absolute source path")
    val compiler = Paths.get(selected.head)
    require(compiler.isAbsolute && Files.isRegularFile(compiler) && Files.isExecutable(compiler),
      "Clang command requires an explicit absolute compiler executable")
    require(!selected.exists(value => Set("-o", "-c", "-S", "-emit-ast", "-fsyntax-only")(value)),
      "Clang command contains competing capture/output flags; remove them from the explicit command")
    require(!selected.exists(value => Set("-load", "-plugin", "-add-plugin")(value) || value.startsWith("-fplugin")),
      "External compiler plugins are unsupported in the capture command; configure only the matching exporter")
    val protectedInputs = (Seq(source, compiler, plugin) ++ input.protectedInputs).distinct.map(protect)
    Files.createDirectories(directory)
    val documentPath = directory.resolve("clang.json")
    val action = if (config.embedNativeEvidence) Seq("-emit-ast", "-o", directory.resolve("compiler-types.ast").toString) else Seq("-fsyntax-only")
    val command      = selected.dropRight(1) ++ action ++ Seq(
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
      documentPath.toString
    ) ++ Seq(
      Option.when(!config.embedNativeEvidence)("direct-cpg-only"),
      Option.when(config.includeComments)("include-comments"),
      Option.when(config.logPreprocessor)("log-preprocessor")
    ).flatten.flatMap(value => Seq("-Xclang", "-plugin-arg-clang-cpg", "-Xclang", value)) :+ source.toString
    Files.writeString(directory.resolve("command.json"), ujson.write(command, indent = 2))
    val started = System.nanoTime()
    val process = new ProcessBuilder(command.asJava)
      .directory(input.directory.toFile)
      .redirectOutput(directory.resolve("compiler.stdout").toFile)
      .redirectError(directory.resolve("compiler.log").toFile)
      .start()
    val exited = try process.waitFor(300, TimeUnit.SECONDS) catch {
      case error: InterruptedException =>
        process.destroyForcibly().waitFor()
        Thread.currentThread().interrupt()
        throw error
    }
    if (!exited) {
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
    if (config.logProblems) System.err.print(Files.readString(directory.resolve("compiler.log")))
    require(
      process.exitValue() == 0 && Files.isRegularFile(documentPath),
      s"Clang frontend failed or did not emit complete evidence; inspect ${directory.resolve("compiler.log")}"
    )
    protectedInputs.foreach(_.validate())
    val document = ujson.read(Files.readString(documentPath))
    require(
      document("schema_version").num == 1 && document("ast")("kind").str == "TranslationUnitDecl",
      "Clang exporter returned an unsupported or incomplete schema; use the matching exporter"
    )
    if (!config.embedNativeEvidence) {
      require(document.obj.get("direct_cpg_only").contains(ujson.Bool(true)), "Clang exporter did not honor direct-cpg-only; use the matching exporter")
      require(!document.obj.contains("compiler_graph") && !document.obj.contains("tokens") &&
        !Files.exists(directory.resolve("compiler-types.ast")) && !Files.exists(directory.resolve("clang.json.tokens")),
        "Direct CPG capture unexpectedly emitted legacy evidence; correct the producer")
    }
    val artifacts = Seq(documentPath) ++ (if (config.embedNativeEvidence) Seq(directory.resolve("compiler-types.ast"), directory.resolve("clang.json.tokens")) else Seq.empty)
    artifacts.foreach(path =>
      require(
        Files.isRegularFile(path) && Files.size(path) > 0,
        s"Clang did not emit required native evidence: $path; rebuild with the matching exporter"
      )
    )
    val evidence = ClangEvidence(source, directory, document)
    evidence.validate()
    val receipt = ujson.Obj(
      "parser" -> "clang", "source" -> source.toString, "selected_command" -> ujson.Arr.from(selected),
      "artifacts" -> ujson.Obj.from(artifacts.map(path => path.getFileName.toString -> ujson.Str(digest(path)))),
      "command_sha256" -> digest(directory.resolve("command.json")), "evidence_sha256" -> digest(documentPath),
      "tools" -> ujson.Obj.from(protectedInputs.map(item => item.target.toString -> ujson.Str(item.sha256))),
      "tool_aliases" -> ujson.Obj.from(protectedInputs.map(item => item.alias.toString -> ujson.Str(item.target.toString)))
    )
    if (!config.embedNativeEvidence) receipt("inputs") = document("inputs")
    Files.writeString(directory.resolve("receipt.json"), ujson.write(receipt, indent = 2))
    evidence
  }
}
