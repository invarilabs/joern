package io.joern.clang2cpg.parser.memory

import io.joern.c2cpg.{C2Cpg, Config}
import io.joern.x2cpg.X2Cpg
import java.nio.file.{Files, Path}

/** Stock comparator, precompiled outside the timed invocation. */
object StockBenchmark {
  def main(arguments: Array[String]): Unit = {
    require(arguments.length == 3, "Expected source-root compilation-database output-directory")
    val root = Path.of(arguments(0)).toRealPath()
    val output = Path.of(arguments(2)).toAbsolutePath.normalize()
    require(!Files.exists(output), "Stock benchmark output exists: " + output)
    Files.createDirectories(output)
    val config = Config(compilationDatabaseFilename = Some(arguments(1))).withInputPath(root.toString)
      .withOutputPath(output.resolve("source.cpg.bin").toString)
    val started = System.nanoTime()
    val graph = new C2Cpg().createCpg(config).get
    try X2Cpg.applyDefaultOverlays(graph) finally graph.close()
    val report = ujson.Obj("status" -> "graph_ready", "backend" -> "stock c2cpg/CDT",
      "graph_seconds" -> ((System.nanoTime() - started).toDouble / 1e9))
    Files.writeString(output.resolve("result.json"), ujson.write(report, indent = 2) + "\n")
    println(ujson.write(report, indent = 2))
  }
}
