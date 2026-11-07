package io.joern.clang2cpg.parser.clang

import io.joern.clang2cpg.Config
import io.joern.clang2cpg.parser.{ClangParser, ClangInputs, ClangCaptures}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.Try

object ClangCpg {
  def create(config: Config): Try[Cpg] = Try(ClangInputs.plan(config)).flatMap { inputs =>
    val selected = Paths.get(config.inputPath).toAbsolutePath.normalize()
    val root = if (Files.isDirectory(selected)) selected else selected.getParent
    val repository = Files.isDirectory(selected)
    withNewEmptyCpg(config.outputPath, config) { (graph, _) =>
      new MetaDataPass(graph, Languages.NEWC, root.toString).createAndApply()
      val bindings = mutable.LinkedHashMap.empty[String, String]
      val inputsAtCapture = mutable.LinkedHashMap.empty[String, String]
      val plugin = ClangInputs.plugin(config)
      val directory = ClangInputs.evidenceDirectory(config)
      ClangCaptures.foreach(inputs.zipWithIndex, config.clangWorkers) { case (input, index) =>
        val captureDirectory = if (repository) directory.resolve(f"tu-$index%05d") else directory
        val started = System.nanoTime()
        val evidence = ClangParser.capture(input, plugin, captureDirectory, config)
        (evidence, (System.nanoTime() - started).toDouble / 1e9)
      } { case ((input, index), (evidence, captureSeconds)) =>
        val captureDirectory = if (repository) directory.resolve(f"tu-$index%05d") else directory
        val receipt = ujson.read(Files.readString(captureDirectory.resolve("receipt.json")))
        (evidence.document("inputs").obj.toSeq ++ receipt("tools").obj.toSeq).foreach { case (path, hash) =>
          require(!inputsAtCapture.contains(path) || inputsAtCapture(path) == hash.str, s"Clang input changed between translation units: $path")
          inputsAtCapture(path) = hash.str
        }
        val graphStarted = System.nanoTime()
        val documentBytes = Files.size(captureDirectory.resolve("clang.json"))
        val syntax = new ClangAstCreationPass(graph, evidence, if (repository) Some(root) else None, config.skipFunctionBodies, !config.disableFileContent, config.embedNativeEvidence)
        syntax.createAndApply()
        syntax.sourceTypeBindings.foreach { case (key, value) =>
          require(!bindings.contains(key) || bindings(key) == value, s"Conflicting compiler type binding: $key")
          bindings(key) = value
        }
        evidence.validate()
        val graphSeconds = (System.nanoTime() - graphStarted).toDouble / 1e9
        val archiveStarted = System.nanoTime()
        if (repository) {
          val archive = ClangArchive.complete(captureDirectory)
          Files.writeString(directory.resolve(f"tu-$index%05d.timings.json"), ujson.write(ujson.Obj(
            "source" -> input.source.toString, "capture_seconds" -> captureSeconds,
            "ast_seconds" -> graphSeconds, "archive_seconds" -> ((System.nanoTime() - archiveStarted).toDouble / 1e9),
            "native_json_bytes" -> ujson.Num(documentBytes.toDouble), "archive_bytes" -> ujson.Num(Files.size(archive).toDouble)
          ), indent = 2))
        }
      }
      inputsAtCapture.foreach { case (name, expected) =>
        val path = Paths.get(name)
        require(Files.isRegularFile(path) && ClangParser.digest(path) == expected,
          s"Clang input changed during repository generation: $path; regenerate the graph")
      }
      new ClangTypeNodePass(graph, bindings.toMap).createAndApply()
    }
  }
}
