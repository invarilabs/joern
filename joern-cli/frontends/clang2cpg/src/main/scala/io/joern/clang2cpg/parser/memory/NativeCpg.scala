package io.joern.clang2cpg.parser.memory

import io.joern.clang2cpg.Config
import io.joern.clang2cpg.parser.{ClangInput, ClangCaptures}
import io.joern.clang2cpg.parser.clang.{ClangStringPool, ClangTagPool, ClangInterTuLinkPass, ClangTypeNodePass, ClangAstEmitter, ClangMacroSites}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Try

/** Experimental explicit JNI entrypoint; no alternate parser or disk evidence path. */
object NativeCpg {
  final case class IncludedDefinition(source: Path, sha256: String)
  final case class Timing(source: String, nativeSeconds: Double, graphSeconds: Double)
  def create(inputs: Seq[ClangInput], root: Path, output: Path, library: Path,
    timings: mutable.Buffer[Timing], repository: Boolean, workers: Int = 1, includedDefinitions: Map[Path, Seq[IncludedDefinition]] = Map.empty): Try[Cpg] = {
    require(workers > 0, "Native capture worker count must be positive")
    require(inputs.nonEmpty, "Native CPG requires explicitly selected translation units")
    require(inputs.map(_.source).distinct.size == inputs.size, "Duplicate native translation unit")
    require(includedDefinitions.keySet.subsetOf(inputs.map(_.source).toSet), "Included definition owner is outside selected translation units")
    includedDefinitions.foreach { case (_, selections) =>
      require(selections.nonEmpty && selections.map(_.source).distinct.size == selections.size, "Empty or duplicate included definition selection")
      selections.foreach { selected =>
        require(selected.source.isAbsolute && Files.isRegularFile(selected.source) && selected.source.toRealPath() == selected.source,
          "Included definition source must be canonical and existing: " + selected.source)
        require(selected.sha256.matches("[0-9a-f]{64}") && NativeGraphEvidence.digest(selected.source) == selected.sha256,
          "Included definition source hash is stale: " + selected.source)
      }
    }
    val native = ClangNative.load(library)
    val config = Config().withInputPath(root.toString).withOutputPath(output.toString)
    withNewEmptyCpg(output.toString, config) { (graph, _) =>
      new MetaDataPass(graph, Languages.NEWC, root.toString).createAndApply()
      val strings = new ClangStringPool
      val tags = new ClangTagPool
      val macroSites = new ClangMacroSites
      val bindings = mutable.LinkedHashMap.empty[String, String]
      val closure = mutable.LinkedHashMap.empty[String, String]
      ClangCaptures.foreach(inputs, workers) { input =>
        val started = System.nanoTime()
        val selected = includedDefinitions.getOrElse(input.source, Seq.empty)
        val document = native.parse(input.command, input.directory, selected.map(_.source))
        selected.foreach { entry =>
          require(document("inputs").obj.get(entry.source.toString).exists(_.str == entry.sha256),
            "Included definition source is absent or stale in compiler input closure: " + entry.source)
        }
        document -> ((System.nanoTime() - started).toDouble / 1e9)
      } { case (input, (document, nativeSeconds)) =>
        val paths = Seq(library.toAbsolutePath.normalize(), Path.of(input.command.head), input.source)
        val aliases = paths.map(path => path.toString -> path.toRealPath().toString).toMap
        val tools = paths.map(path => path.toRealPath().toString -> NativeGraphEvidence.digest(path.toRealPath())).toMap
        val evidence = NativeGraphEvidence(input.source, document, input.command, tools, aliases)
        evidence.validate()
        document("inputs").obj.foreach { case (name, digest) =>
          require(!closure.contains(name) || closure(name) == digest.str, "Native input changed between translation units: " + name)
          closure(name) = digest.str
        }
        val graphStart = System.nanoTime()
        val syntax = new ClangAstEmitter(graph, evidence, if (repository) Some(root) else None,
          includeFileContent = true, embedNativeEvidence = false, strings = strings, tags = tags, macroSites = Some(macroSites))
        syntax.createAndApply()
        syntax.publishTags()
        syntax.sourceTypeBindings.foreach { case (name, identity) =>
          require(!bindings.contains(name) || bindings(name) == identity, "Contradictory native type binding: " + name)
          bindings(name) = identity
        }
        evidence.validate()
        timings += Timing(input.source.toString, nativeSeconds, (System.nanoTime() - graphStart).toDouble / 1e9)
      }
      closure.foreach { case (name, expected) =>
        require(Files.isRegularFile(Path.of(name)) && NativeGraphEvidence.digest(Path.of(name)) == expected,
          "Native input changed during repository generation: " + name)
      }
      new ClangInterTuLinkPass(graph).createAndApply()
      new ClangTypeNodePass(graph, bindings.toMap).createAndApply()
    }
  }
}
