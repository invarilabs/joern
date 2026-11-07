package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.memory.{ClangNative, NativeCpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{ControlStructure, JumpTarget, Tag}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

class NativeParityTests extends AnyWordSpec with Matchers {
  private val loopSource = """
    int fold(int n) {
      int sum=0;
      for(int i=0;i<n;i++){if(i==2)continue;sum+=i;}
      while(n>0){n--;if(n==1)break;}
      do{sum++;}while(sum<3);
      return sum+n;
    }
    """
  private def setup(code: String): (Path, Path) = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "jni-parity-")
    val source = directory.resolve("sample.c")
    Files.writeString(source, code)
    directory -> source
  }
  private def create(directory: Path, source: Path): Cpg = {
    val compiler = sys.env("CLANG_CPG_COMPILER")
    val input = ClangInput(source, Seq(compiler, "-std=gnu11", source.toString), directory)
    NativeCpg.create(Seq(input), directory, directory.resolve("native.cpg.bin"),
      Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")), mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = false).get
  }
  private def withGraph(code: String)(check: Cpg => Unit): Unit = {
    val (directory, source) = setup(code)
    val graph = create(directory, source)
    try check(graph) finally graph.close()
  }
  private def control(graph: Cpg, category: String): ControlStructure =
    graph.controlStructure.controlStructureTypeExact(category).head
  private def tag(node: io.shiftleft.codepropertygraph.generated.nodes.StoredNode, name: String): String = {
    val facts = node._taggedByOut.collect { case value: Tag if value.name == name => value.value }.toList
    require(facts.size == 1, "Expected exactly one " + name + " fact")
    facts.head
  }

  "Direct native C feature parity" should {
    "preserve command-line macro provenance without treating a compiler buffer as a disk file" in {
      val (directory, source) = setup("int read(void){return COMMAND_VALUE;}")
      val input = ClangInput(source, Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", "-DCOMMAND_VALUE=7", source.toString), directory)
      val graph = NativeCpg.create(Seq(input), directory, directory.resolve("native.cpg.bin"),
        Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY")), mutable.ArrayBuffer.empty[NativeCpg.Timing], repository = false).get
      try {
        graph.method.nameExact("COMMAND_VALUE").filename.toList shouldBe List("<compiler>")
        graph.method.nameExact("read").ast.isLiteral.code.toList should contain("7")
      } finally graph.close()
    }

    "retain compound literal initialization and accesses through declared record fields" in {
      withGraph("struct Cell{int left;int right;};int read(int input){struct Cell value=(struct Cell){.right=2,.left=input};return value.left+value.right;}") { graph =>
        val literal = graph.call.nameExact(Operators.cast).head
        literal.argument.size shouldBe 2
        literal.argument.argumentIndex(1).isTypeRef.size shouldBe 1
        literal.argument.argumentIndex(2).isCall.name.toList shouldBe List("<operator>.arrayInitializer")
        literal.argument.argumentIndex(2).isCall.argument.size shouldBe 2
        val accesses = graph.method.nameExact("read").ast.isCall.nameExact(Operators.fieldAccess).toList
        accesses.size shouldBe 2
        accesses.foreach(_.argument.size shouldBe 2)
        accesses.flatMap(_.argument.isFieldIdentifier.canonicalName).toSet shouldBe Set("left", "right")
        graph.method.nameExact("read").ast.isIdentifier.name.toList should contain("input")
      }
    }

    "retain designated array elements, compiler zero filling and indexed reads" in {
      withGraph("int read(void){int values[4]={[2]=7};return values[2];}") { graph =>
        val array = graph.call.nameExact("<operator>.arrayInitializer").head
        array.ast.isLiteral.code.toList should contain("7")
        val access = graph.call.nameExact(Operators.indexAccess).head
        access.argument.argumentIndex(1).isIdentifier.name.toList shouldBe List("values")
        access.argument.argumentIndex(2).isLiteral.code.toList shouldBe List("2")
        graph.tag.nameExact("clang.arrayFiller").size shouldBe 1
        graph.local.nameExact("values").typeFullName.toList should not contain("ANY")
      }
    }

    "preserve all for while and do loop control roles and executable updates" in {
      withGraph(loopSource) { graph =>
        val counted = control(graph, "FOR")
        counted.out(EdgeTypes.FOR_INIT).size shouldBe 1
        counted.out(EdgeTypes.CONDITION).size shouldBe 1
        counted.out(EdgeTypes.FOR_UPDATE).size shouldBe 1
        counted.out(EdgeTypes.FOR_BODY).size shouldBe 1
        control(graph, "WHILE").out(EdgeTypes.TRUE_BODY).size shouldBe 1
        control(graph, "DO").out(EdgeTypes.DO_BODY).size shouldBe 1
        control(graph, "DO").out(EdgeTypes.CONDITION).size shouldBe 1
        graph.controlStructure.controlStructureTypeExact("CONTINUE").size shouldBe 1
        graph.controlStructure.controlStructureTypeExact("BREAK").size shouldBe 1
        graph.call.nameExact(Operators.assignmentPlus).size shouldBe 1
        graph.call.nameExact(Operators.postIncrement).size shouldBe 2
        graph.call.nameExact(Operators.postDecrement).size shouldBe 1
      }
    }

    "preserve omitted loop clauses without inventing conditions or updates after renaming" in {
      Seq("run" -> "stop", "consume" -> "signal").foreach { case (method, parameter) =>
        withGraph(s"int $method(int $parameter){for(;;){if($parameter)break;else return 4;}return $parameter;}") { graph =>
          val loop = control(graph, "FOR")
          loop.out(EdgeTypes.FOR_INIT).size shouldBe 0
          loop.out(EdgeTypes.CONDITION).size shouldBe 0
          loop.out(EdgeTypes.FOR_UPDATE).size shouldBe 0
          loop.out(EdgeTypes.FOR_BODY).size shouldBe 1
          graph.method.nameExact(method).ast.isReturn.size shouldBe 2
          graph.method.nameExact(method).ast.isIdentifier.name.toList should contain(parameter)
        }
      }
    }

    "retain switch fallthrough default break and goto targets for CFG construction" in {
      withGraph("int choose(int op){int value=0;switch(op){case 0:value+=1;case 1:value+=2;break;default:goto cleanup;}value+=4;cleanup:return value;}") { graph =>
        control(graph, "SWITCH").out(EdgeTypes.CONDITION).size shouldBe 1
        control(graph, "SWITCH").out(EdgeTypes.TRUE_BODY).size shouldBe 1
        graph.jumpTarget.nameExact("case").size shouldBe 2
        graph.jumpTarget.nameExact("default").size shouldBe 1
        graph.jumpTarget.nameExact("cleanup").size shouldBe 1
        val jump = control(graph, "GOTO")
        jump.out(EdgeTypes.JUMP_ARGUMENT).size shouldBe 1
        graph.call.nameExact(Operators.assignmentPlus).size shouldBe 3
        X2Cpg.applyDefaultOverlays(graph)
        jump.cfgNext.collect { case target: JumpTarget => target.name }.toList shouldBe List("cleanup")
        val update = graph.call.nameExact(Operators.assignmentPlus)
          .find(_.code.replaceAll("\\s", "") == "value+=1").get
        update.cfgNext.collect { case target: JumpTarget => target.name }.toList shouldBe List("case")
      }
    }

    "retain typed variadic extraction and both list management calls" in {
      withGraph("#include <stdarg.h>\nint take(int count,...){va_list args;va_start(args,count);int result=va_arg(args,int);va_end(args);return result;}") { graph =>
        val extract = graph.call.nameExact("<operator>.vaArg").head
        extract.argument.size shouldBe 2
        extract.argument.argumentIndex(2).isTypeRef.typeFullName.toList shouldBe List("int")
        graph.call.nameExact("__builtin_va_start").size shouldBe 1
        graph.call.nameExact("__builtin_va_end").size shouldBe 1
        graph.local.nameExact("args").size shouldBe 1
        graph.local.nameExact("result").size shouldBe 1
      }
    }

    "emit only compiler-selected generic behavior and omit unevaluated calls" in {
      withGraph("int side(void);int selected(int);int other(int);int choose(int value){return _Generic(side(),int:selected,default:other)(value);}") { graph =>
        graph.call.nameExact("selected").size shouldBe 1
        graph.call.nameExact("side").size shouldBe 0
        graph.call.nameExact("other").size shouldBe 0
        graph.call.nameExact("<operator>.pointerCall").size shouldBe 0
        graph.tag.nameExact("clang.genericSelection").size shouldBe 1
        graph.call.nameExact("selected").argument.isIdentifier.name.toList shouldBe List("value")
      }
    }

    "preserve atomic source argument ordering and compiler operand roles" in {
      withGraph("int *location(void);int next(void);int order(void);int exchange(void){return __atomic_exchange_n(location(),next(),order());}") { graph =>
        val exchange = graph.call.nameExact("__atomic_exchange_n").head
        exchange.argument.toList.sortBy(_.argumentIndex).map(_.asInstanceOf[io.shiftleft.codepropertygraph.generated.nodes.Call].name) shouldBe
          List("location", "next", "order")
        val facts = ujson.read(tag(exchange, "clang.atomic"))
        facts("operands").arr.map(_("role").str).toList shouldBe List("pointer", "value", "order")
        graph.call.nameExact("location").size shouldBe 1
        graph.call.nameExact("next").size shouldBe 1
        graph.call.nameExact("order").size shouldBe 1
      }
    }

    "retain nested GNU statement expression declarations and final values" in {
      withGraph("int calculate(int input){return ({int temporary=input+1;({int nested=temporary*2;nested+3;});});}") { graph =>
        val blocks = graph.block.filter(node => node._taggedByOut.exists {
          case fact: Tag => fact.name == "clang.kind" && fact.value == "StmtExpr"
          case _ => false
        }).toList
        blocks.size shouldBe 2
        blocks.foreach(_.typeFullName shouldBe "int")
        graph.local.nameExact("temporary").size shouldBe 1
        graph.local.nameExact("nested").size shouldBe 1
        graph.call.nameExact(Operators.addition).size shouldBe 2
        graph.call.nameExact(Operators.multiplication).size shouldBe 1
      }
    }

    "match stock c2cpg operators and controls using an explicit separate comparator JVM" in {
      val (directory, source) = setup(loopSource)
      val graph = create(directory, source)
      try {
        val testClasses = Paths.get(classOf[NativeParityTests].getProtectionDomain.getCodeSource.getLocation.toURI)
        val mainClasses = Paths.get(classOf[NativeCpg.Timing].getProtectionDomain.getCodeSource.getLocation.toURI)
        val runtime = Paths.get(sys.env("JOERN_RUNTIME"))
        val classpath = Seq(testClasses.toString, mainClasses.toString,
          runtime.resolve("frontends/c2cpg/lib/*").toString).mkString(java.io.File.pathSeparator)
        val report = directory.resolve("stock-projection.json")
        val log = directory.resolve("stock-comparator.log")
        val command = Seq(Paths.get(sys.props("java.home"), "bin", "java").toString, "-cp", classpath,
          "io.joern.clang2cpg.parser.NativeStockFeatureProjection", source.toString, report.toString)
        val status = new ProcessBuilder(command*)
          .redirectErrorStream(true).redirectOutput(log.toFile).start().waitFor()
        withClue("Stock comparator failed; inspect " + log + ": ") { status shouldBe 0 }
        val expected = ujson.read(Files.readString(report))
        val method = graph.method.nameExact("fold").head
        method.ast.isCall.name.filter(_.startsWith("<operator>.")).toList.sorted shouldBe
          expected("operators").arr.map(_.str).toList
        method.ast.isControlStructure.controlStructureType.toList.sorted shouldBe
          expected("controls").arr.map(_.str).toList
      } finally graph.close()
    }
  }
}

/** Stock comparison is intentionally isolated from the process executing the JNI frontend. */
object NativeStockFeatureProjection {
  def main(arguments: Array[String]): Unit = {
    require(arguments.length == 2, "Expected selected-source projection-report")
    val source = Paths.get(arguments(0)).toRealPath()
    val report = Paths.get(arguments(1)).toAbsolutePath.normalize()
    require(!Files.exists(report), "Stock projection report already exists")
    val config = io.joern.c2cpg.Config().withInputPath(source.toString)
      .withOutputPath(report.resolveSibling("stock.cpg.bin").toString)
    val graph = new io.joern.c2cpg.C2Cpg().createCpg(config).get
    try {
      val method = graph.method.nameExact("fold").head
      Files.writeString(report, ujson.write(ujson.Obj(
        "backend" -> "stock c2cpg/CDT in a separate comparator process",
        "operators" -> method.ast.isCall.name.filter(_.startsWith("<operator>.")).toList.sorted,
        "controls" -> method.ast.isControlStructure.controlStructureType.toList.sorted
      ), indent = 2) + "\n")
    } finally graph.close()
  }
}
