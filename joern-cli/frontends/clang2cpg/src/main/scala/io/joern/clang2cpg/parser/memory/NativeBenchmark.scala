package io.joern.clang2cpg.parser.memory

import io.joern.clang2cpg.Config
import io.joern.clang2cpg.parser.ClangInputs
import io.joern.x2cpg.X2Cpg
import java.nio.file.{Files, Path}
import scala.collection.mutable

/** Explicit prototype harness; all selected files share one CPG, native library and JVM. */
object NativeBenchmark {
  def main(arguments: Array[String]): Unit = {
    require(arguments.length == 5, "Expected source-root compilation-database output-directory JNI-library repository|single")
    val root = Path.of(arguments(0)).toRealPath()
    val output = Path.of(arguments(2)).toAbsolutePath.normalize()
    require(!Files.exists(output), "Benchmark output already exists: " + output)
    Files.createDirectories(output)
    val config = Config(compilationDatabaseFilename = Some(arguments(1))).withInputPath(root.toString)
    val inputs = ClangInputs.plan(config)
    val repository = arguments(4) match {
      case "repository" => true
      case "single" => require(inputs.size == 1, "Single-file benchmark requires exactly one source"); false
      case other => throw new IllegalArgumentException("Unknown benchmark scope: " + other)
    }
    val graphRoot = if (Files.isDirectory(root)) root else root.getParent
    val timings = mutable.ArrayBuffer.empty[NativeCpg.Timing]
    val started = System.nanoTime()
    try {
      val graph = NativeCpg.create(inputs, graphRoot, output.resolve("source.cpg.bin"), Path.of(arguments(3)), timings, repository).get
      try X2Cpg.applyDefaultOverlays(graph) finally graph.close()
      val report = ujson.Obj("status" -> "graph_ready", "source_count" -> inputs.size,
        "graph_seconds" -> ((System.nanoTime() - started).toDouble / 1e9),
        "graph_sha256" -> NativeGraphEvidence.digest(output.resolve("source.cpg.bin")),
        "transport" -> "JNI structured memory; no AST dump or JSON reader",
        "files" -> ujson.Arr.from(timings.map(value => ujson.Obj("source" -> value.source,
          "native_seconds" -> value.nativeSeconds, "graph_seconds" -> value.graphSeconds))))
      Files.writeString(output.resolve("result.json"), ujson.write(report, indent = 2) + "\n")
      println(ujson.write(report, indent = 2))
    } catch {
      case failure: Throwable =>
        Files.writeString(output.resolve("result.json"), ujson.write(ujson.Obj("status" -> "blocked",
          "error" -> failure.toString, "completed_translation_units" -> timings.size,
          "seconds" -> ((System.nanoTime() - started).toDouble / 1e9)), indent = 2) + "\n")
        throw failure
    }
  }
}
