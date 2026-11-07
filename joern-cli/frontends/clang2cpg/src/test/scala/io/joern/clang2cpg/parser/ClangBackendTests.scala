package io.joern.clang2cpg.parser

import io.joern.clang2cpg.{Clang2Cpg, Config, Frontend}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scopt.OParser
import java.nio.file.{Files, Path, Paths}

class ClangBackendTests extends AnyWordSpec with Matchers {
  private def setup(code: String): (Path, Config) = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-cpg-test-")
    val source    = directory.resolve("sample.c")
    Files.writeString(source, code)
    val compiler =
      sys.env.getOrElse("CLANG_CPG_COMPILER", fail("Set CLANG_CPG_COMPILER to the matching Clang executable"))
    val plugin  = sys.env.getOrElse("CLANG_CPG_PLUGIN", fail("Set CLANG_CPG_PLUGIN to the built native exporter"))
    val command = directory.resolve("command.json")
    Files.writeString(command, ujson.write(Seq(compiler, "-std=gnu11", source.toString)))
    val config = OParser
      .parse(
        Frontend.cmdLineParser,
        Seq(
          "--parser",
          "clang",
          "--clang-command",
          command.toString,
          "--clang-plugin",
          plugin,
          "--clang-evidence-dir",
          directory.resolve("evidence").toString
        ),
        Config().withInputPath(source.toString)
      )
      .getOrElse(fail("Clang parser options rejected"))
    directory -> config
  }
  private def withGraph(code: String)(test: (Cpg, Path) => Unit): Unit = {
    val (directory, config) = setup(code)
    val graph               = new Clang2Cpg().createCpg(config).get
    try {
      X2Cpg.applyDefaultOverlays(graph)
      test(graph, directory)
    } finally graph.close()
  }
  "Native Clang CPG" should {
    "retain compiler-concatenated string values across source and macro spelling" in {
      withGraph("#define TAIL \"tail\"\nconst char *f(void){return \"head\" TAIL;}") { (graph, _) =>
        val literal = graph.literal.head
        literal.code shouldBe "\"headtail\""
        val strings = literal._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.stringLiteral" => ujson.read(tag.value)
        }.toList
        strings.size shouldBe 1
        strings.head("units").arr.map(_.num.toInt.toChar).mkString shouldBe "headtail"
      }
    }

    "preserve atomic operand roles and source argument order" in {
      withGraph("int *ptr(void); int value(void); int order(void); int update(void){return __atomic_fetch_add(ptr(),value(),order());}") { (graph, _) =>
        val atomic = graph.call.nameExact("<operator>.atomic.__atomic_fetch_add").head
        atomic.argument.argumentIndex(1).isCall.name.toList shouldBe List("ptr")
        atomic.argument.argumentIndex(2).isCall.name.toList shouldBe List("value")
        atomic.argument.argumentIndex(3).isCall.name.toList shouldBe List("order")
        atomic.typeFullName shouldBe "int"
        atomic._taggedByOut.exists {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag => tag.name == "clang.atomic"
          case _ => false
        } shouldBe true
      }
    }

    "emit predefined strings from the compiler literal value" in {
      withGraph("const char *named(void){return __func__;} const char *pretty(void){return __PRETTY_FUNCTION__;}") { (graph, _) =>
        val values = graph.literal.toList.filter(node => node._taggedByOut.exists {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag => tag.name == "clang.kind" && tag.value == "PredefinedExpr"
          case _ => false
        })
        values.size shouldBe 2
        values.map(_.code) should contain ("\"named\"")
        values.foreach(node => node._taggedByOut.exists {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag => tag.name == "clang.stringLiteral"
          case _ => false
        } shouldBe true)
      }
    }

    "preserve compiler-checked assertions without creating runtime message values" in {
      withGraph("_Static_assert(sizeof(int)>=2, \"int width\"); int f(void){_Static_assert(1, \"local proof\");return 1;}") { (graph, _) =>
        val proofs = graph.tag.nameExact("clang.staticAssertion").value.toList.map(ujson.read(_))
        proofs.size shouldBe 2
        proofs.forall(proof => !proof("failed").bool && !proof("dependent").bool) shouldBe true
        graph.literal.code.toList should not contain("\"local proof\"")
      }
    }

    "emit only the compiler-selected generic association without evaluating its controller" in {
      withGraph("int side(void); int f(int x) {return _Generic(side(), int:x+1, default:side());}") { (graph, _) =>
        graph.call.nameExact("side").size shouldBe 0
        graph.call.nameExact("<operator>.addition").size shouldBe 1
        graph.tag.nameExact("clang.genericSelection").size shouldBe 1
      }
    }
    "resolve generic-selected callees through compiler expression identities" in {
      withGraph("int selected(int); int other(int); int f(int x) {return _Generic(x, int:selected, default:other)(x);}") { (graph, _) =>
        graph.call.nameExact("selected").size shouldBe 1
        graph.call.nameExact("other").size shouldBe 0
        graph.call.nameExact("<operator>.pointerCall").size shouldBe 0
      }
    }

    "retain the value and source marker of GNU extension expressions" in {
      withGraph("int f(int x) {return __extension__ (x + 1);}") { (graph, _) =>
        graph.call.nameExact("<operator>.addition").size shouldBe 1
        graph.tag.nameExact("clang.extensionMarker").value.toList shouldBe List("__extension__ (x + 1)")
      }
    }

    "type compiler-generated omitted record initializers" in {
      withGraph("struct Pair {int x;int y;}; struct Pair p={1}; int read(void) {return p.y;}") { (graph, _) =>
        val omitted = graph.call.nameExact("<operator>.implicitValueInit").toList
        omitted.size shouldBe 1
        omitted.head.typeFullName shouldBe "int"
        omitted.head._taggedByOut.exists {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag => tag.name == "clang.semanticType"
          case _ => false
        } shouldBe true
      }
    }

    "retain GNU statement expressions as typed value blocks with their declarations" in {
      withGraph("int f(void) {return ({int a=1; a+2;});}") { (graph, _) =>
        val block = graph.block.filter(node => node._taggedByOut.exists {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag => tag.name == "clang.kind" && tag.value == "StmtExpr"
          case _ => false
        }).head
        block.typeFullName shouldBe "int"
        block.ast.isLocal.name.toList shouldBe List("a")
        block.ast.isCall.name.toList should contain ("<operator>.addition")
      }
    }

    "reject unsupported C++ declarations instead of silently producing an empty graph" in {
      val (directory, config) = setup("struct A {int x; A():x(1){}};")
      Files.writeString(directory.resolve("command.json"), ujson.write(Seq(
        sys.env("CLANG_CPG_COMPILER"), "-x", "c++", "-std=c++17", directory.resolve("sample.c").toString
      )))
      val failure = new Clang2Cpg().createCpg(config).failed.get
      Iterator.iterate(failure: Throwable)(_.getCause).takeWhile(_ != null).map(_.toString).mkString(" ") should include("Unsupported Clang declaration CXX")
    }

    "retain source file content only when explicitly requested" in {
      val code = "int f(void) {return 1;}"
      val (_, config) = setup(code)
      val graph = new Clang2Cpg().createCpg(config.withDisableFileContent(false)).get
      try graph.file.nameExact("sample.c").content.toList shouldBe List(code)
      finally graph.close()
    }

    "retain C switch labels and loop exits" in {
      withGraph("int f(int x) {while(x) {--x; if(x == 3) continue; switch(x) {case 1: break; default: return x;}} return 0;}") { (graph, _) =>
        graph.controlStructure.controlStructureType.toList should contain allOf ("BREAK", "CONTINUE", "SWITCH", "WHILE")
        graph.jumpTarget.name.toList should contain allOf ("case", "default")
      }
    }

    "retain function-like empty macro invocations and source directive events" in {
      withGraph("#define EMPTY(x)\n#define VALUE 7\nint read(void) {EMPTY(1); return VALUE;}") { (graph, _) =>
        val contract = ujson.read(graph.tag.nameExact("clang.translationUnit").value.head)
        val sites    = contract("macro_sites").arr
        sites.find(_("name").str == "EMPTY").get("function_like").bool shouldBe true
        sites.find(_("name").str == "VALUE").get("function_like").bool shouldBe false
        contract("macro_directives").arr.map(_("name").str).toList shouldBe List("EMPTY", "VALUE")
        val method = graph.method.nameExact("read").head
        val owned  = method._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag
              if tag.name == "clang.functionMacroInvocations" =>
            ujson.read(tag.value)
        }.toList
        owned.size shouldBe 1
        owned.head.arr.map(_("name").str).toList shouldBe List("EMPTY")
        val wrappers = graph.call
          .nameExact("VALUE")
          .head
          ._taggedByOut
          .collect {
            case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.macroInvocation" =>
              ujson.read(tag.value)
          }
          .toList
        wrappers.size shouldBe 1
        wrappers.head("name").str shouldBe "VALUE"
      }
    }

    "separate written casts from typed implicit conversions" in {
      withGraph(
        "typedef struct Node Node; Node *empty(void) {return ((void*)0);} double number(int count) {return count;}"
      ) { (graph, _) =>
        val written = graph.call.nameExact("<operator>.cast").head
        written._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.writtenCastKind" =>
            tag.value
        }.toList shouldBe List("NullToPointer")
        val conversions = graph.tag.nameExact("clang.implicitConversion").value.toList.map(ujson.read(_))
        val numeric     = conversions.filter(_("kind").str == "IntegralToFloating")
        numeric.size shouldBe 1
        numeric.head("source_type")("kind").str shouldBe "Int"
        numeric.head("target_type")("kind").str shouldBe "Double"
        numeric.head("target_type")("value_spelling").str shouldBe "double"
        val target = written._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.semanticType" =>
            ujson.read(tag.value)
        }.toList
        target.head("value_spelling").str shouldBe "void *"
      }
    }

    "emit canonical physical identities for source and header inputs" in {
      val (directory, config) =
        setup("#include \"nested/../types.h\"\n#include \"alias.h\"\nint read(Count count) {return count;}")
      Files.createDirectory(directory.resolve("nested"))
      Files.writeString(directory.resolve("types.h"), "#pragma once\ntypedef int Count;\n")
      Files.createSymbolicLink(directory.resolve("alias.h"), Paths.get("types.h"))
      val native = ClangParser.capture(config)
      val inputs = native.document("inputs").obj
      inputs.keys.foreach(name => Paths.get(name).toRealPath().toString shouldBe name)
      inputs.keySet should contain(directory.resolve("types.h").toRealPath().toString)
      inputs.keySet should not contain (directory.resolve("alias.h").toString)
      native.validate()
    }

    "type every generated initializer target using its compiler declaration" in {
      withGraph("static int cached=3; int read(void) {int value=cached; return value;}") { (graph, _) =>
        val targets = graph.call.nameExact("<operator>.assignment").argument(1).isIdentifier.toList
        targets.size shouldBe 2
        targets.foreach { target =>
          val types = target._taggedByOut.collect {
            case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.semanticType" =>
              ujson.read(tag.value)
          }.toList
          types.size shouldBe 1
          types.head("kind").str shouldBe "Int"
          target.refOut.size shouldBe 1
        }
      }
    }

    "retain semantic declaration types and values on imported proxies" in {
      withGraph(
        "typedef struct {int value;} Box; enum Phase { READY=7 }; extern int (*handler)(int); int run(Box *box) {return handler(READY)+box->value;}"
      ) {
        (graph, _) =>
          val contracts = graph.tag.nameExact("clang.translationUnit").value.toList
        contracts.size shouldBe 1
        ujson.read(contracts.head)("schema_version").num shouldBe 2
        val parameter = graph.method.nameExact("run").parameter.nameExact("box").head
        parameter._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.semanticType" =>
            ujson.read(tag.value)("pointee")("declaration_name").str
        }.toList shouldBe List("Box")
        val imported = graph.method.nameExact("run").local.toList.filter(_.code.startsWith("<global> "))
        imported.map(_.name).toSet shouldBe Set("READY", "handler")
        imported.foreach { local =>
          local._taggedByOut
            .collect {
              case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.semanticType" =>
                ujson.read(tag.value)
            }
            .toList
            .size shouldBe 1
        }
        val ready = imported.find(_.name == "READY").get
        ready._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.constantValue" =>
            tag.value
        }.toList shouldBe List("7")
        val handler = imported.find(_.name == "handler").get
        handler._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.initializerKind" =>
            tag.value
        }.toList shouldBe List("external")
      }
    }

    "attach native structural type descriptors directly to the CPG" in {
      withGraph(
        "typedef struct Box {int value;} Box; typedef Box Alias; int read(Alias *box, int (*callback)(int)) {return callback(box->value);}"
      ) { (graph, directory) =>
        val contract = ujson.read(graph.tag.nameExact("clang.translationUnit").value.head)
        val command  = directory.resolve("command.json")
        contract("tools")(command.toString).str shouldBe ClangParser.digest(command)
        val parameter = graph.method.nameExact("read").parameter.nameExact("box").head
        val types     = parameter._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.semanticType" =>
            ujson.read(tag.value)
        }.toList
        types.size shouldBe 1
        types.head("kind").str shouldBe "Pointer"
        types.head("pointee")("kind").str shouldBe "Record"
        types.head("pointee")("declaration_usr").str should not be empty
        val callback = graph.method.nameExact("read").parameter.nameExact("callback").head
        val callable = callback._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.semanticType" =>
            ujson.read(tag.value)
        }.toList
        callable.size shouldBe 1
        callable.head("pointee")("kind").str shouldBe "FunctionProto"
        callable.head("pointee")("result")("kind").str shouldBe "Int"
        callable.head("pointee")("arguments").arr.map(_("kind").str).toList shouldBe List("Int")
        val payload = ujson.read(graph.tag.nameExact("clang.nativeEvidence").value.head)
        payload("schema_version").num shouldBe 1
        payload.obj.contains("fact_package") shouldBe false
        Files.exists(directory.resolve("evidence/frontend-facts.json")) shouldBe false
      }
    }

    "emit a renamed ordinary function directly into the semantic CPG" in {
      withGraph("int shifted(int amount) {return amount + 3;}") { (graph, _) =>
        graph.method.nameExact("shifted").size shouldBe 1
        graph.method.nameExact("shifted").parameter.nameExact("amount").typeFullName.l shouldBe List("int")
        graph.method.nameExact("shifted").methodReturn.typeFullName.l shouldBe List("int")
        graph.call.nameExact("<operator>.addition").typeFullName.l shouldBe List("int")
      }
    }

    "store native structural types and shadowed declaration bindings in the CPG" in {
      withGraph(
        "typedef struct Node {int value;} Node; typedef Node Alias; int read(Alias x) {typedef double Alias; Alias y=1; return x.value+(int)y;}"
      ) { (graph, _) =>
        val payload = ujson.read(graph.tag.nameExact("clang.nativeEvidence").value.toList match {
          case List(value) => value
          case _           => fail("one native compiler evidence payload is required")
        })
        val native = payload("compiler_graph")
        native("schema_version").num shouldBe 1
        val declarations = native("nodes").obj.values
          .filter(node => node("spelling").str == "Alias" && node("kind").str == "TypedefDecl")
          .toList
        declarations.size shouldBe 2
        val types     = native("types").obj
        val canonical = declarations.map(node => types(types(node("type").str)("canonical").str)("kind").str).toSet
        canonical shouldBe Set("Record", "Double")
        declarations.map(_("usr").str).distinct.size shouldBe 2
        payload("tokens").str should include("typedef")
      }
    }

    "preserve field/index types and correct C logical-not results" in {
      withGraph(
        "typedef struct State {double values[3];} State; double read(State *p,int i) {return p->values[i];} int truth(double d) {return !d;}"
      ) { (graph, _) =>
        graph.call.nameExact("<operator>.indirectFieldAccess").typeFullName.toSet shouldBe Set("double[3]")
        graph.call.nameExact("<operator>.indexAccess").typeFullName.toSet shouldBe Set("double")
        graph.call.nameExact("<operator>.logicalNot").typeFullName.toSet shouldBe Set("int")
        graph.call.typeFullNameExact("ANY").size shouldBe 0
      }
    }
    "project implicit conversions as evidence while retaining written casts as operations" in {
      withGraph("double convert(int x) {double result=x; return (double)x + result;}") { (graph, _) =>
        val casts = graph.call.nameExact("<operator>.cast").l
        casts.size shouldBe 1
        casts.head.code shouldBe "(double)x"
        graph.tag.nameExact("clang.implicitConversion").value.l.exists(_.contains("IntegralToFloating")) shouldBe true
        graph.identifier.nameExact("x").typeFullName.toSet shouldBe Set("int")
      }
    }
    "retain source types for method results and sizeof operands" in {
      withGraph(
        "typedef struct Node {int value;} Node; Node *read(Node *node) {return node;} unsigned long size(void) {return sizeof(Node);}"
      ) { (graph, _) =>
        graph.method.nameExact("read").methodReturn.typeFullName.l shouldBe List("Node *")
        graph.call.nameExact("<operator>.sizeOf").argument.isTypeRef.code.l shouldBe List("Node")
      }
    }
    "retain compiler source type spelling separately from canonical record identity" in {
      withGraph("typedef struct Node {int value;} Node; int read(Node *node) {return node->value;}") { (graph, _) =>
        graph.method.nameExact("read").parameter.nameExact("node").typeFullName.l shouldBe List("Node *")
        graph.typeDecl.nameExact("Node").filter(_.aliasTypeFullName.isEmpty).fullName.l.head should startWith("clang:")
        graph.call.nameExact("<operator>.indirectFieldAccess").typeFullName.l shouldBe List("int")
      }
    }
    "resolve colliding tag and typedef records using compiler declaration identity" in {
      withGraph(
        "typedef struct Shared {int unrelated;} Logged; typedef struct {double durations[3];} Shared; typedef struct Owner {Shared stats;} Owner; double read(Owner *p) {return p->stats.durations[0];}"
      ) { (graph, _) =>
        val members = graph.member.nameExact("stats").l
        members.size shouldBe 1
        val targets = members.head._evalTypeOut.flatMap(_._refOut).map(_.asInstanceOf[TypeDecl]).toList
        targets.size shouldBe 1
        targets.head.member.name.toSet shouldBe Set("durations")
      }
    }
    "preserve typed enum conditionals and ordinary indirect calls" in {
      withGraph(
        "typedef enum Mode {READ=0,WRITE=1} Mode; struct Ctx {Mode flags;}; Mode pick(struct Ctx *ctx) {return ctx ? ctx->flags : READ;} int run(int (*handler)(int),int x) {return handler(x);}"
      ) { (graph, _) =>
        graph.call.nameExact("<operator>.conditional").typeFullName.l.size shouldBe 1
        graph.call.nameExact("<operator>.conditional").typeFullName.l.head should not be "ANY"
        val indirect = graph.call.nameExact("<operator>.pointerCall").l
        indirect.size shouldBe 1
        indirect.head.argument.argumentIndex.toSet shouldBe Set(-1, 1)
        indirect.head.typeFullName shouldBe "int"
      }
    }
    "retain evaluated character values, strings and implicit conversion evidence" in {
      withGraph(
        "const char *message=\"A\\x42\" \"C\"; int letters(void) {return 'AB';} double convert(int x) {double y=x; return y;}"
      ) { (graph, directory) =>
        val native = ujson.read(Files.readString(directory.resolve("evidence/clang.json")))
        def nodes(node: ujson.Value): Seq[ujson.Value] =
          Seq(node) ++ node.obj.get("inner").toSeq.flatMap(_.arr).flatMap(nodes)
        val syntax = nodes(native("ast"))
        syntax.filter(node => node("kind").str == "CharacterLiteral").map(_("value").num) shouldBe Seq(16706.0)
        syntax.filter(node => node("kind").str == "StringLiteral").map(_("value").str) should contain("\"ABC\"")
        graph.tag.nameExact("clang.castKind").value.toSet should contain("IntegralToFloating")
        graph.literal.codeExact("'AB'").typeFullName.l shouldBe List("int")
        val strings = graph.literal.toList.flatMap(literal =>
          literal._taggedByOut.collect {
            case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.stringLiteral" =>
              ujson.read(tag.value)
          }
        )
        strings.size shouldBe 1
        strings.head("unit_width").num shouldBe 1
        strings.head("units").arr.map(_.num.toInt).toList shouldBe List(65, 66, 67)
        graph.call.nameExact("<operator>.assignment").code.l.exists(_.contains("message")) shouldBe true
      }
    }
    "preserve source globals as file locals and indirect-call receiver roles" in {
      withGraph("static int state; int run(int (*callback)(int)) {state=callback(state); return state;}") {
        (graph, _) =>
          graph.method.nameExact("<global>").ast.isLocal.nameExact("state").size shouldBe 1
          val call = graph.call.nameExact("<operator>.pointerCall").l
          call.size shouldBe 1
          call.head.argument.argumentIndex.toSet shouldBe Set(-1, 1)
          call.head.argument.argumentIndex(-1).isIdentifier.name.l shouldBe List("callback")
      }
    }
    "link repeated macro expansions to exactly one declaration each" in {
      withGraph("#define NUMBER 7\nint first(void) {return NUMBER;} int second(void) {return NUMBER;}") { (graph, _) =>
        val calls = graph.call.nameExact("NUMBER").l
        calls.size shouldBe 2
        calls.foreach(call => call._callOut.size shouldBe 1)
      }
    }
    "retain expanded literal spellings with their compiler provenance" in {
      withGraph(
        "#define NUMBER 7UL\n#define MESSAGE \"hello\"\nunsigned long number(void) {return NUMBER;} const char *message(void) {return MESSAGE;}"
      ) { (graph, _) =>
        graph.literal.code.toSet shouldBe Set("7UL", "\"hello\"")
      }
    }
    "reject missing required variable USRs instead of retaining process addresses" in {
      val (_, config) = setup("extern int state; int read(void) {return state;}")
      val native      = ClangParser.capture(config)
      native
        .document("nodes")
        .obj
        .values
        .filter(_.obj.get("name").contains(ujson.Str("state")))
        .foreach(_.obj.remove("usr"))
      val graph = Cpg.empty
      try {
        val failure = intercept[Exception] {
          new io.joern.clang2cpg.parser.clang.ClangAstCreationPass(graph, native).createAndApply()
        }
        Iterator
          .iterate(failure: Throwable)(_.getCause)
          .takeWhile(_ != null)
          .map(_.toString)
          .mkString(" ")
          .should(include("compiler USR"))
      } finally graph.close()
    }
    "retain stable compiler identities across native captures" in {
      val (directory, config) =
        setup("#define SCALAR int\nextern int shared; int read(void) {SCALAR copied=shared; return copied;}")
      def identities(configuration: Config): List[String] = {
        val graph = new Clang2Cpg().createCpg(configuration).get
        try
          graph.tag
            .filter(tag => Set("clang.declarationMacro", "clang.declarationIdentity")(tag.name))
            .map(tag => tag.name + ":" + tag.value)
            .l
            .sorted
        finally graph.close()
      }
      val first = identities(config)
      // Every declaration now carries its identity, in addition to the
      // imported-value proxy and declaration-macro provenance.
      first.size shouldBe 6
      val imported =
        first.find(_.startsWith("clang.declarationIdentity:")).get.stripPrefix("clang.declarationIdentity:")
      imported should startWith("c:")
      val declaration = first.find(_.startsWith("clang.declarationMacro:")).get.stripPrefix("clang.declarationMacro:")
      ujson.read(declaration)("declaration").str should startWith("c:")
      identities(config.copy(clangEvidenceDir = Some(directory.resolve("evidence-second").toString))) shouldBe first
    }
    "retain sole declaration macro provenance without replacing typed declarations" in {
      withGraph(
        "#define SCALAR int\n#define VALUE 1\nstruct State {int value;}; int read(struct State *state) {SCALAR copied=state->value; SCALAR combined=VALUE; return copied+combined;}"
      ) { (graph, _) =>
        val copies = graph.local.nameExact("copied").l
        copies.size shouldBe 1
        copies.head._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.declarationMacro" =>
            ujson.read(tag.value)("name").str
        }.toList shouldBe List("SCALAR")
        graph.local
          .nameExact("combined")
          .l
          .head
          ._taggedByOut
          .collect {
            case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.declarationMacro" => tag
          }
          .size shouldBe 0
      }
    }
    "retain a macro source invocation with its complete expansion" in {
      withGraph("#define AS_PTR ((void *)0)\nint empty(void *value) {return value == AS_PTR;}") { (graph, _) =>
        val calls = graph.call.nameExact("AS_PTR").l
        calls.size shouldBe 1
        calls.head.dispatchType shouldBe "INLINED"
        calls.head.astChildren.isBlock.astChildren.isCall.name.l shouldBe List("<operator>.cast")
        graph.method.fullNameExact(calls.head.methodFullName).code.l.head should startWith("#define AS_PTR")
      }
    }
    "export reusable native evidence without another compiler invocation" in {
      withGraph("#define VALUE 7\nint read(void) {return VALUE;}") { (_, directory) =>
        Files.size(directory.resolve("evidence/compiler-types.ast")) should be > 0L
        Files.readString(directory.resolve("evidence/clang.json.tokens")) should include("numeric_constant '7'")
        val native = ujson.read(Files.readString(directory.resolve("evidence/clang.json")))
        native("macro_names").arr.map(_.str) should contain("VALUE")
        native("predefines").str should include("#define __STDC__")
        native("predefines").str should not include "#define VALUE"
      }
    }
    "retain macro occurrences, expanded tokens and fresh header closure" in {
      withGraph("#define TWICE(x) ((x)+(x))\nint read(int x) {return TWICE(x);}") { (graph, directory) =>
        val native = ujson.read(Files.readString(directory.resolve("evidence/clang.json")))
        native("macros").arr.map(_("name").str) should contain("TWICE")
        native("tokens").arr.count(_.obj.get("text").exists(_.str == "x")) should be >= 3
        native("inputs").obj should not be empty
        graph.call.nameExact("<operator>.addition").typeFullName.l shouldBe List("int")
      }
    }
    "preserve compiler-declared builtin calls without a name-based semantic rule" in {
      withGraph("long predict(int x) {return __builtin_expect(x,0);}") { (graph, _) =>
        graph.call.nameExact("__builtin_expect").typeFullName.l shouldBe List("long")
      }
    }
    "preserve explicit branch and goto roles for graph overlays" in {
      withGraph("int step(int x) {if(x) goto done; return 0; done: return 1;}") { (graph, _) =>
        graph.controlStructure.controlStructureTypeExact("IF").flatMap(_._trueBodyOut).size shouldBe 1
        val jumps = graph.controlStructure.controlStructureTypeExact("GOTO").l
        jumps.size shouldBe 1
        jumps.head._jumpArgumentOut.size shouldBe 1
        jumps.head.cfgNext.collect { case target: io.shiftleft.codepropertygraph.generated.nodes.JumpTarget =>
          target.name
        }.toList shouldBe List("done")
      }
    }
    "retain compiler-created parameters of typeof function declarations" in {
      withGraph(
        "int original(int,double); extern __typeof__(original) alias; int invoke(void) {return alias(1,2.0);}"
      ) { (graph, _) =>
        graph.method.nameExact("alias").parameter.size shouldBe 2
        graph.call.nameExact("alias").typeFullName.l shouldBe List("int")
      }
    }
    "register translation-unit scope alongside the first selected definition" in {
      withGraph("static int state; int first(void) {return state;} int second(void) {return state;}") { (graph, _) =>
        graph.file
          .nameExact("sample.c")
          .method
          .filterNot(_.isExternal)
          .toList
          .sortBy(_.id)
          .map(_.name)
          .shouldBe(List("first", "<global>", "second"))
      }
    }
    "order selected definitions by source location rather than header prototype order" in {
      withGraph("int later(void); int earlier(void) {return later();} int later(void) {return 1;}") { (graph, _) =>
        graph.file
          .nameExact("sample.c")
          .method
          .filter(method => !method.isExternal && method.name != "<global>")
          .toList
          .sortBy(_.id)
          .map(_.name) shouldBe List("earlier", "later")
      }
    }
    "preserve compatibility with source-function selection using canonical compiler identities" in {
      withGraph("int selected(void) {return 1;}") { (graph, _) =>
        graph.file
          .nameExact("sample.c")
          .method
          .filter(method =>
            !method.isExternal && !method.name.startsWith("<") && !method.fullName.startsWith("sample.c:")
          )
          .name
          .l shouldBe List("selected")
      }
    }
    "reject invalid compiler input and retain the diagnostics" in {
      val (directory, config) = setup("int invalid(void) {int *p=1.5; return missing_name;}")
      val result              = new Clang2Cpg().createCpg(config)
      result.isFailure shouldBe true
      Files.readString(directory.resolve("evidence/compiler.log")) should include("error:")
    }
    "reject unsupported executable syntax instead of emitting UNKNOWN or dropping it" in {
      val (_, config) = setup("void unsupported(void) {asm volatile (\"\");}")
      val failure     = new Clang2Cpg().createCpg(config).failed.get
      failure.toString should include("Unsupported Clang")
    }
    "bind imported enumerators inside the selected source method" in {
      val (directory, config) = setup("#include \"mode.h\"\nint read(void) {return WAITING;}")
      Files.writeString(directory.resolve("mode.h"), "enum Mode {WAITING=3};")
      val graph = new Clang2Cpg().createCpg(config).get
      try {
        val targets = graph.identifier.nameExact("WAITING").refOut.l
        targets.size shouldBe 1
        targets.head.isInstanceOf[io.shiftleft.codepropertygraph.generated.nodes.Local] shouldBe true
      } finally graph.close()
    }
    "canonicalize physical macro definition paths in graph files" in {
      val (directory, config) = setup("#include \"nested/../value.h\"\nint read(void) {return VALUE;}")
      Files.createDirectory(directory.resolve("nested"))
      Files.writeString(directory.resolve("value.h"), "#define VALUE 3\n")
      val graph = new Clang2Cpg().createCpg(config).get
      try graph.file.name.l.foreach(name => Paths.get(name).normalize().toString shouldBe name)
      finally graph.close()
    }
    "keep header-local variables out of the global member inventory" in {
      val (directory, config) = setup("#include \"helper.h\"\nint value(void) {return helper();}")
      Files.writeString(
        directory.resolve("helper.h"),
        "static inline int helper(void); static inline int helper(void) {int temporary=1; return temporary;}"
      )
      val graph = new Clang2Cpg().createCpg(config).get
      try graph.member.nameExact("temporary").size shouldBe 0
      finally graph.close()
    }
    "retain explicit array elements and compiler-provided implicit fill evidence" in {
      withGraph("int values[4]={1}; int read(void) {return values[0];}") { (graph, _) =>
        val initializer = graph.call.nameExact("<operator>.arrayInitializer").head
        initializer.argument.code.toList shouldBe List("1")
        val fill = initializer._taggedByOut.collect {
          case tag: io.shiftleft.codepropertygraph.generated.nodes.Tag if tag.name == "clang.arrayFiller" => ujson.read(tag.value)
        }.toList
        fill.size shouldBe 1
        fill.head("type")("extent").num.toInt shouldBe 4
        fill.head("kind").str shouldBe "ImplicitValueInitExpr"
      }
    }
    "reject reuse after a source change" in {
      val (directory, config) = setup("int value(void) {return 1;}")
      val native              = ClangParser.capture(config)
      Files.writeString(directory.resolve("sample.c"), "int value(void) {return 2;}")
      intercept[IllegalArgumentException](native.validate()).getMessage should include("stale")
    }
    "reject incomplete native identity coverage instead of producing an empty graph" in {
      val (_, config) = setup("int value(void) {return 1;}")
      val native      = ClangParser.capture(config)
      native.document("nodes") = ujson.Obj()
      val graph = Cpg.empty
      try {
        val error = intercept[Exception] {
          new io.joern.clang2cpg.parser.clang.ClangAstCreationPass(graph, native).createAndApply()
        }
        Iterator
          .iterate(error: Throwable)(_.getCause)
          .takeWhile(_ != null)
          .map(_.toString)
          .mkString(" ") should include("native evidence")
      } finally graph.close()
    }
    "reject missing required backend configuration without invoking CDT" in {
      val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-cpg-missing-")
      val source    = directory.resolve("sample.c")
      Files.writeString(source, "int value(void) {return 1;}")
      val failure =
        new Clang2Cpg().createCpg(Config().withInputPath(source.toString).copy(parserBackend = "clang")).failed.get
      failure.getMessage should include("--clang-plugin")
    }
  }
}
