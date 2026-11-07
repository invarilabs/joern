package io.joern.clang2cpg.parser.clang

import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewNode, NewTag, Tag}
import scala.collection.mutable

/** Identical metadata is shared within one graph; ordered passes publish only applied stored tags. */
final class ClangTagPool {
  private type Key = (String, String)
  private val stored = mutable.HashMap.empty[Key, Tag]
  private var owner: Option[flatgraph.Graph] = None
  private var active: Option[Pass] = None

  def beginPass(graph: Cpg): Pass = {
    require(graph != null && !graph.graph.isClosed, "Clang tag pool requires an open graph")
    owner.foreach(previous => require(previous eq graph.graph, "Clang tag pool cannot share tags with a different graph"))
    require(active.isEmpty, "Clang tag pool has an unpublished pass")
    owner = Some(graph.graph)
    val pass = new Pass(graph.graph)
    active = Some(pass)
    pass
  }

  final class Pass private[ClangTagPool] (graph: flatgraph.Graph) {
    private val pending = mutable.LinkedHashMap.empty[Key, NewTag]
    private val associations = new java.util.IdentityHashMap[NewNode, mutable.HashSet[Key]]()
    private var published = false

    private def validateActive(): Unit =
      require(!published && active.exists(_ eq this), "Clang tag pass is no longer active")

    def attach(builder: DiffGraphBuilder, node: NewNode, name: String, value: String): Unit = {
      validateActive()
      require(builder != null && node != null, "Clang tag association requires a builder and source node")
      require(name != null && value != null, "Clang tag names and values cannot be null")
      node.storedRef.foreach(existing => require(existing.graph eq graph, "Clang tag source belongs to a different graph"))
      val key = name -> value
      var attached = associations.get(node)
      if (attached == null) {
        attached = mutable.HashSet.empty[Key]
        associations.put(node, attached)
      }
      if (attached.add(key)) {
        stored.get(key) match {
          case Some(tag) =>
            require(tag.graph eq graph, "Clang stored tag belongs to a different graph")
            builder.addEdge(node, tag, EdgeTypes.TAGGED_BY)
          case None =>
            val tag = pending.getOrElseUpdate(key, {
              val created = NewTag().name(name).value(value)
              builder.addNode(created)
              created
            })
            builder.addEdge(node, tag, EdgeTypes.TAGGED_BY)
        }
      }
    }

    /** Must follow applying the pass diff; never retain source NewNodes in the graph-wide cache. */
    def publish(): Unit = {
      validateActive()
      val resolved = pending.iterator.map { case (key, node) =>
        val tag = node.storedRef match {
          case Some(value: Tag) => value
          case _ => throw new IllegalArgumentException("Clang tag publication has unresolved pending tags; apply the diff first")
        }
        require(tag.graph eq graph, "Clang pending tag was applied to a different graph")
        require(!stored.contains(key), "Clang tag publication would replace an existing stored tag")
        key -> tag
      }.toVector
      val sources = associations.keySet().iterator()
      while (sources.hasNext) {
        val node = sources.next()
        require(node.storedRef.exists(value => value.graph eq graph),
          "Clang tag publication has unresolved source associations; apply the diff to the owning graph first")
      }
      resolved.foreach { case (key, tag) => stored(key) = tag }
      pending.clear()
      associations.clear()
      published = true
      active = None
    }
  }
}
