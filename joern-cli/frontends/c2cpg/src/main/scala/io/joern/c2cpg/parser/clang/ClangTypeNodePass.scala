package io.joern.c2cpg.parser.clang

import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewType

/** Source spellings and canonical declarations are separate compiler facts. */
class ClangTypeNodePass(graph: Cpg, bindings: Map[String, String])
    extends TypeNodePass(Set.empty, graph, getTypesFromCpg = true) {
  override def run(builder: DiffGraphBuilder): Unit = {
    val names = (typeDeclTypes ++ typeFullNamesFromCpg).toSeq.sorted
    names.filterNot(name => name == "<empty>" || name.endsWith(":<global>")).foreach { name =>
      builder.addNode(
        NewType()
          .name(fullToShortName(name))
          .fullName(name)
          .typeDeclFullName(bindings.getOrElse(name, name))
      )
    }
  }
}
