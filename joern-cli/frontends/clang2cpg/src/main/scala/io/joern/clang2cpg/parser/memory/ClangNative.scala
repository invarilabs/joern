package io.joern.clang2cpg.parser.memory

import java.nio.file.{Files, Path}

/** Explicit in-process Clang entrypoint. Loading or compiler errors propagate; no alternate parser is used. */
final class ClangNative private () {
  @native private def parse0(command: Array[String], cwd: String): java.util.Map[String, Object]

  def parse(command: Seq[String], cwd: Path): NativeValue = {
    require(command != null && command.size >= 2, "Native Clang requires a compiler command and source input")
    require(command.forall(value => value != null && value.nonEmpty), "Native Clang command has an empty argument")
    require(cwd != null && cwd.isAbsolute && Files.isDirectory(cwd),
      "Native Clang requires an existing absolute working directory")
    val document = NativeValue.fromJava(parse0(command.toArray, cwd.toRealPath().toString))
    require(document.objOpt.nonEmpty, "Native Clang returned a non-object translation unit")
    require(document("schema_version").num == 1, "Native Clang returned an unsupported schema")
    require(document("direct_cpg_only").bool, "Native Clang did not return direct CPG facts")
    require(document("ast")("kind").str == "TranslationUnitDecl", "Native Clang returned an incomplete translation unit")
    require(document("nodes").obj.nonEmpty, "Native Clang returned no compiler node facts")
    require(document("inputs").obj.nonEmpty, "Native Clang returned no source/header closure")
    document
  }
}

object ClangNative {
  def load(library: Path): ClangNative = {
    require(library != null && library.isAbsolute && Files.isRegularFile(library),
      "Native Clang requires an explicitly selected existing absolute JNI library")
    System.load(library.toRealPath().toString)
    new ClangNative()
  }
}
