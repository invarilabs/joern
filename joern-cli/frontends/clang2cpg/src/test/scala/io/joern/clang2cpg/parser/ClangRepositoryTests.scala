package io.joern.clang2cpg.parser

import io.joern.clang2cpg.{Clang2Cpg, Config}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Paths}

class ClangRepositoryTests extends AnyWordSpec with Matchers {
  "Repository Clang CPG" should {
    "keep same-basename translation units and their header declarations isolated" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-repo-")
      val header = root.resolve("shared.h")
      Files.writeString(header, "typedef struct Shared { int value; } Shared;\n")
      val sources = Seq("left", "right").map { name =>
        val dir = Files.createDirectory(root.resolve(name))
        val source = dir.resolve("common.c")
        Files.writeString(source, "#include \"../shared.h\"\nstatic int helper(Shared v) {return v.value;} int " + name + "(Shared v) {return helper(v);}")
        source
      }
      val db = root.resolve("compile_commands.json")
      Files.writeString(db, ujson.write(sources.map(source => ujson.Obj(
        "directory" -> root.toString, "file" -> source.toString,
        "arguments" -> ujson.Arr(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", "-c", source.toString)
      ))))
      val config = Config().withInputPath(root.toString).copy(
        compilationDatabaseFilename = Some(db.toString),
        clangPlugin = Some(sys.env("CLANG_CPG_PLUGIN")),
        clangEvidenceDir = Some(root.resolve("evidence").toString)
      )
      val graph = new Clang2Cpg().createCpg(config).get
      try {
        X2Cpg.applyDefaultOverlays(graph)
        graph.file.name.toSet should contain allOf ("left/common.c", "right/common.c")
        val helpers = graph.method.nameExact("helper").toList
        helpers.size shouldBe 2
        helpers.map(_.fullName).distinct.size shouldBe 2
        graph.call.nameExact("helper").toList.foreach { call =>
          call._callOut.collect { case method: io.shiftleft.codepropertygraph.generated.nodes.Method => method.filename }.toList shouldBe List(call.method.filename)
        }
        val records = graph.typeDecl.nameExact("Shared").filter(_.isExternal).toList
        records.size shouldBe 2
        records.map(_.fullName).distinct.size shouldBe 2
        records.foreach { record =>
          record._taggedByOut.count { case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag => tag.name == "clang.translationUnitSource"; case _ => false } shouldBe 1
        }
      } finally graph.close()
    }
  }
}
