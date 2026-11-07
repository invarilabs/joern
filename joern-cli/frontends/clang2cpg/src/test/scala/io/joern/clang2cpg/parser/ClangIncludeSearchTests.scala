package io.joern.clang2cpg.parser

import io.joern.clang2cpg.{Clang2Cpg, Config}
import io.shiftleft.semanticcpg.language.*
import java.nio.file.{Files, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClangIncludeSearchTests extends AnyWordSpec with Matchers {
  "Compiler-owned include search" should {
    "expose actual search groups in the CPG without changing the selected command" in {
      val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-search-graph-")
      val quoted = Files.createDirectory(root.resolve("quoted paths"))
      val angled = Files.createDirectory(root.resolve("angled paths"))
      val system = Files.createDirectory(root.resolve("system paths"))
      Files.writeString(angled.resolve("answer.h"), "#define ANSWER 7\n")
      val source = root.resolve("sample.c")
      Files.writeString(source, "#include <answer.h>\nint answer(void){return ANSWER;}\n")
      val command = root.resolve("command.json")
      val argv = Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", "-nostdinc", "-iquote", quoted.toString,
        "-I", angled.toString, "-isystem", system.toString, source.toString)
      Files.writeString(command, ujson.write(argv))
      val config = Config(clangCommand=Some(command.toString), clangPlugin=Some(sys.env("CLANG_CPG_PLUGIN")),
        clangEvidenceDir=Some(root.resolve("evidence").toString), includePathsAutoDiscovery=true,
        embedNativeEvidence=false).withInputPath(source.toString)
      val graph = new Clang2Cpg().createCpg(config).get
      try {
        val facts = graph.tag.nameExact("clang.includeSearch").value.toList
        facts.size shouldBe 1
        val search = ujson.read(facts.head)
        search("compiler_owned").bool shouldBe true
        search("entries").arr.map(_("path").str).toList shouldBe List(quoted.toString, angled.toString, system.toString)
        search("entries").arr.map(_("group").str).toList shouldBe List("quoted", "angled", "system")
        val receipt = ujson.read(Files.readString(root.resolve("evidence/receipt.json")))
        receipt("selected_command").arr.map(_.str).toSeq shouldBe argv
      } finally graph.close()
    }
    "reject missing or unrequested compiler search evidence" in {
      val absent = ujson.Obj()
      intercept[IllegalArgumentException](ClangParser.validateIncludeSearch(absent, true)).getMessage should include("include search")
      ClangParser.validateIncludeSearch(absent, false)
      val complete = ujson.Obj("include_search" -> ujson.Obj("schema_version" -> 1, "compiler_owned" -> true, "working_directory" -> "/build", "entries" -> ujson.Arr()))
      ClangParser.validateIncludeSearch(complete, true)
      intercept[IllegalArgumentException](ClangParser.validateIncludeSearch(complete, false)).getMessage should include("unrequested")
    }
    "reject invalid search metadata instead of substituting paths" in {
      def document(kind: String, group: String, path: String): ujson.Value = ujson.Obj("include_search" ->
        ujson.Obj("schema_version" -> 1, "compiler_owned" -> true, "working_directory" -> "/build", "entries" -> ujson.Arr(
          ujson.Obj("path" -> path, "kind" -> kind, "group" -> group, "characteristic" -> 0))))
      Seq(document("unknown", "angled", "/include"), document("directory", "unknown", "/include"),
        document("directory", "angled", "")).foreach(value =>
          intercept[IllegalArgumentException](ClangParser.validateIncludeSearch(value, true)))
    }
  }
}
