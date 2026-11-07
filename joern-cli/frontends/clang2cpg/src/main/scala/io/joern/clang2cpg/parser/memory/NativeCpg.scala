package io.joern.clang2cpg.parser.memory

import io.joern.clang2cpg.Config
import io.joern.clang2cpg.parser.ClangInput
import io.joern.clang2cpg.parser.clang.{ClangStringPool, ClangTagPool, ClangInterTuLinkPass, ClangTypeNodePass}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Try

/** Experimental explicit JNI entrypoint; no alternate parser or disk evidence path. */
object NativeCpg {
  final case class Timing(source: String, nativeSeconds: Double, graphSeconds: Double)
  def create(inputs: Seq[ClangInput], root: Path, output: Path, library: Path,
    timings: mutable.Buffer[Timing], repository: Boolean): Try[Cpg] = {
    require(inputs.nonEmpty, "Native CPG requires explicitly selected translation units")
    require(inputs.map(_.source).distinct.size == inputs.size, "Duplicate native translation unit")
    val native = ClangNative.load(library)
    val config = Config().withInputPath(root.toString).withOutputPath(output.toString)
    withNewEmptyCpg(output.toString, config) { (graph, _) =>
      new MetaDataPass(graph, Languages.NEWC, root.toString).createAndApply()
      val strings = new ClangStringPool
      val tags = new ClangTagPool
      val bindings = mutable.LinkedHashMap.empty[String, String]
      val closure = mutable.LinkedHashMap.empty[String, String]
      inputs.foreach { input =>
        val started = System.nanoTime()
        val document = native.parse(input.command, input.directory)
        val nativeSeconds = (System.nanoTime() - started).toDouble / 1e9
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
        val syntax = new NativeAstCreationPass(graph, evidence, if (repository) Some(root) else None,
          includeFileContent = true, embedNativeEvidence = false, strings = strings, tags = tags)
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
