package io.joern.clang2cpg.parser.clang

import io.joern.clang2cpg.Config
import io.joern.clang2cpg.parser.{ClangParser, ClangInputs}
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
      val plugin = ClangInputs.plugin(config)
      val directory = ClangInputs.evidenceDirectory(config)
      inputs.zipWithIndex.foreach { case (input, index) =>
        val captureDirectory = if (repository) directory.resolve(f"tu-$index%05d") else directory
        val evidence = ClangParser.capture(input, plugin, captureDirectory, config)
        val syntax = new ClangAstCreationPass(graph, evidence, if (repository) Some(root) else None, config.skipFunctionBodies)
        syntax.createAndApply()
        syntax.sourceTypeBindings.foreach { case (key, value) =>
          require(!bindings.contains(key) || bindings(key) == value, s"Conflicting compiler type binding: $key")
          bindings(key) = value
        }
        evidence.validate()
        if (repository) ClangArchive.complete(captureDirectory)
      }
      new ClangTypeNodePass(graph, bindings.toMap).createAndApply()
    }
  }
}
