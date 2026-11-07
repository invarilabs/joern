package io.joern.clang2cpg.parser.clang

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Method, StoredNode, Tag}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** Resolve external declarations by compiler identity, leaving source-call facts intact. */
class ClangInterTuLinkPass(graph: Cpg) extends CpgPass(graph) {
  private def annotation(method: StoredNode, name: String): String = {
    val values = method._taggedByOut.collect {case tag: Tag if tag.name == name => tag.value}.toList.distinct
    require(values.size == 1, s"Clang ${method.label} ${method.id} requires one $name fact")
    values.head
  }
  override def run(builder: DiffGraphBuilder): Unit = {
    val native = graph.method.filter(method => method._taggedByOut.exists {
      case tag: Tag => tag.name == "clang.kind" && Set("FunctionDecl", "CXXMethodDecl")(tag.value)
      case _ => false
    }).toList
    val visible = native.filter { method =>
      annotation(method,"clang.externallyVisible") match {
        case "true" => true
        case "false" => false
        case other => throw new IllegalArgumentException("Invalid compiler external-linkage fact: " + other)
      }
    }
    val targets = visible.groupBy(method =>
      (annotation(method,"clang.declarationIdentity"),annotation(method,"clang.canonicalType"))
    ).values.flatMap { methods =>
      val definitions = methods.filterNot(_.isExternal)
      require(definitions.size <= 1, "Ambiguous externally visible Clang definitions: " + methods.map(_.fullName).sorted.mkString(", "))
      definitions.headOption.toList.flatMap(definition => methods.map(_.fullName -> definition))
    }.toMap
    graph.call.foreach { call =>
      targets.get(call.methodFullName).filter(_.fullName != call.methodFullName).foreach { definition =>
        require(annotation(call,"clang.callCanonicalMethodFullName").nonEmpty, "Clang inter-TU call has an empty source identity")
        require(annotation(call,"clang.callDeclaredFunctionType").nonEmpty, "Clang inter-TU call has an empty source signature")
        require(call._callOut.isEmpty, "Clang inter-TU linking must run before call overlays")
        builder.setNodeProperty(call,PropertyNames.MethodFullName,definition.fullName)
      }
    }
    graph.methodRef.foreach { reference =>
      targets.get(reference.methodFullName).filter(_.fullName != reference.methodFullName).foreach { definition =>
        val old = reference.outE(EdgeTypes.REF).toList
        require(old.size == 1 && old.head.dst.isInstanceOf[Method], "Clang method reference must have one original method REF")
        require(old.head.dst.asInstanceOf[Method].fullName == reference.methodFullName, "Clang method reference identity disagrees with REF")
        builder.removeEdge(old.head)
        builder.addEdge(reference,definition,EdgeTypes.REF)
        builder.setNodeProperty(reference,PropertyNames.MethodFullName,definition.fullName)
      }
    }
  }
}
