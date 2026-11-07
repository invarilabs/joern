package io.joern.clang2cpg.parser.clang

import io.joern.clang2cpg.Config
import io.joern.clang2cpg.parser.ClangParser
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import scala.util.Try

object ClangCpg {
  def create(config: Config): Try[Cpg] = Try(ClangParser.capture(config)).flatMap { evidence =>
    withNewEmptyCpg(config.outputPath, config) { (graph, _) =>
      new MetaDataPass(graph, Languages.NEWC, evidence.source.getParent.toString).createAndApply()
      val syntax = new ClangAstCreationPass(graph, evidence)
      syntax.createAndApply()
      new ClangTypeNodePass(graph, syntax.sourceTypeBindings.toMap).createAndApply()
      evidence.validate()
    }
  }
}
