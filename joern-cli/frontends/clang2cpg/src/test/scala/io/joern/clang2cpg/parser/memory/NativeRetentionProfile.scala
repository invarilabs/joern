package io.joern.clang2cpg.parser.memory

import io.joern.clang2cpg.parser.ClangInputs
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import javax.management.ObjectName
import scala.collection.mutable

/** Explicit diagnostic harness; no compiler evidence is reconstructed or omitted. */
object NativeRetentionProfile {
  def main(arguments: Array[String]): Unit = {
    require(arguments.length == 5, "Expected root database output library native|stock")
    val root = Path.of(arguments(0)).toRealPath()
    val output = Path.of(arguments(2)).toAbsolutePath.normalize()
    require(!Files.exists(output), "Profile output already exists")
    Files.createDirectories(output)
    val config = io.joern.clang2cpg.Config(compilationDatabaseFilename = Some(arguments(1)))
      .withInputPath(root.toString)
    lazy val inputs = ClangInputs.plan(config)
    val selectedCount = ujson.read(Files.readString(Path.of(arguments(1)))).arr.size
    val runtime = Runtime.getRuntime
    val timings = new mutable.ArrayBuffer[NativeCpg.Timing] {
      override def addOne(value: NativeCpg.Timing): this.type = {
        super.addOne(value)
        System.gc()
        val progress = ujson.Obj("completed" -> size, "total" -> selectedCount, "source" -> value.source,
          "native_seconds" -> value.nativeSeconds, "graph_seconds" -> value.graphSeconds,
          "heap_used_bytes_after_gc" -> (runtime.totalMemory() - runtime.freeMemory()).toDouble)
        Files.writeString(output.resolve("progress.jsonl"), ujson.write(progress) + "\n",
          java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
        println(ujson.write(progress))
        this
      }
    }
    val started = System.nanoTime()
    val graph: Cpg = arguments(4) match {
      case "native" => NativeCpg.create(inputs, root, output.resolve("source.cpg.bin"), Path.of(arguments(3)),
        timings, repository = true, workers = 1).get
      case "stock" =>
        val stock = io.joern.c2cpg.Config(compilationDatabaseFilename = Some(arguments(1)))
          .withInputPath(root.toString).withOutputPath(output.resolve("source.cpg.bin").toString)
        new io.joern.c2cpg.C2Cpg().createCpg(stock).get
      case other => throw new IllegalArgumentException("Unknown explicit profile backend: " + other)
    }
    try {
      val histogram = ManagementFactory.getPlatformMBeanServer.invoke(
        new ObjectName("com.sun.management:type=DiagnosticCommand"), "gcClassHistogram",
        Array[AnyRef](Array.empty[String]), Array("[Ljava.lang.String;")).asInstanceOf[String]
      Files.writeString(output.resolve("live-class-histogram.txt"), histogram)
      val labels = mutable.TreeMap.empty[String, Long]
      graph.all.foreach(node => labels(node.label) = labels.getOrElse(node.label, 0L) + 1L)
      val tags = mutable.TreeMap.empty[String, (Long, Long)]
      graph.tag.foreach { node =>
        val (count, chars) = tags.getOrElse(node.name, (0L, 0L))
        tags(node.name) = (count + 1L, chars + node.value.length.toLong)
      }
      val identities = mutable.HashMap.empty[String, Long]
      graph.method.foreach { node =>
        node._taggedByOut.collect { case tag: Tag if tag.name == "clang.declarationIdentity" => tag.value }
          .foreach(identity => identities(identity) = identities.getOrElse(identity, 0L) + 1L)
      }
      val report = ujson.Obj("backend" -> arguments(4), "selected_sources" -> selectedCount,
        "seconds_including_profile_gc" -> ((System.nanoTime() - started).toDouble / 1e9),
        "heap_used_bytes_after_gc" -> (runtime.totalMemory() - runtime.freeMemory()).toDouble,
        "nodes_by_label" -> ujson.Obj.from(labels.iterator.map { case (label, count) => label -> ujson.Num(count.toDouble) }),
        "tags_by_name" -> ujson.Arr.from(tags.iterator.map { case (name, (count, chars)) =>
          ujson.Obj("name" -> name, "count" -> count.toDouble, "utf16_characters" -> chars.toDouble) }),
        "external_methods" -> graph.method.isExternal(true).size,
        "method_compiler_identities" -> identities.size,
        "duplicate_method_declarations" -> identities.valuesIterator.map(value => value - 1L).sum.toDouble)
      Files.writeString(output.resolve("result.json"), ujson.write(report, indent = 2) + "\n")
      println(ujson.write(report, indent = 2))
    } finally graph.close()
  }
}
