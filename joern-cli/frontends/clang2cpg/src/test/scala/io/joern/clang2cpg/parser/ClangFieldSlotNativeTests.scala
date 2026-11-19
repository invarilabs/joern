package io.joern.clang2cpg.parser

import io.joern.clang2cpg.parser.clang.ClangAstEmitter
import io.joern.clang2cpg.parser.memory.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets
import scala.util.Using
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.collection.mutable

class ClangFieldSlotNativeTests extends AnyWordSpec with Matchers {
  private val library  = Paths.get(sys.env("CLANG_CPG_JNI_LIBRARY"))
  private val compiler = sys.env("CLANG_CPG_COMPILER")
  private val fixture  = Using.resource(getClass.getResourceAsStream("/field-slots/input.c")) { stream =>
    require(stream != null, "Missing required field-slot source fixture")
    new String(stream.readAllBytes(), StandardCharsets.UTF_8)
  }
  private def input(code: String): ClangInput = {
    val directory = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "field-slots-")
    val source    = directory.resolve("sample.c")
    Files.writeString(source, code)
    ClangInput(source, Seq(compiler, "-std=gnu11", source.toString), directory)
  }
  private def tag(node: StoredNode, name: String): String = {
    val values = node._taggedByOut.collect { case entry: Tag if entry.name == name => entry.value }.toList
    values.size shouldBe 1
    values.head
  }
  private def owner(node: StoredNode): String = ujson.read(tag(node, "clang.semanticType"))("declaration_usr").str
  private def fields(record: TypeDecl, expected: Seq[(String, Option[Int])]): Seq[String] = {
    val members = record.member.toList.sortBy(_.order)
    members.size shouldBe expected.size
    val keys = members.zip(expected).zipWithIndex.map { case ((member, (role, width)), index) =>
      val key = tag(member, "clang.fieldKey")
      key shouldBe owner(record) + "#field:" + index
      tag(member, "clang.fieldRole") shouldBe role
      ujson.read(tag(member, "clang.isBitfield")).bool shouldBe width.nonEmpty
      width match {
        case Some(value) => ujson.read(tag(member, "clang.bitWidth")).num shouldBe value.toDouble
        case None        =>
          member._taggedByOut.collect { case entry: Tag if entry.name == "clang.bitWidth" => entry }.size shouldBe 0
      }
      key
    }
    keys.distinct.size shouldBe keys.size
    keys.zip(expected).collect { case (key, (role, _)) if role != "unnamed_padding" => key }
  }
  private def withGraph(code: String)(check: Cpg => Unit): Unit = {
    val selected = input(code)
    val graph    = NativeCpg
      .create(
        Seq(selected),
        selected.directory,
        selected.directory.resolve("graph.bin"),
        library,
        mutable.ArrayBuffer.empty[NativeCpg.Timing],
        repository = false
      )
      .get
    try check(graph)
    finally graph.close()
  }
  private def emit(selected: ClangInput, document: NativeValue): Cpg = {
    val graph   = Cpg.empty
    val paths   = Seq(library, Path.of(compiler), selected.source)
    val tools   = paths.map(path => path.toRealPath().toString -> NativeGraphEvidence.digest(path.toRealPath())).toMap
    val aliases = paths.map(path => path.toString -> path.toRealPath().toString).toMap
    try {
      new ClangAstEmitter(
        graph,
        NativeGraphEvidence(selected.source, document, selected.command, tools, aliases),
        embedNativeEvidence = false
      ).createAndApply()
      graph
    } catch {
      case error: Throwable => graph.close(); throw error
    }
  }
  "Native field and semantic slot certificates" should {
    "retain canonical field identities, padding widths, named bits and anonymous carriers after renaming" in {
      for (rename <- Seq(false, true)) {
        val code =
          if (rename)
            fixture
              .replace("RenamedClock", "OtherClock")
              .replace("ticks", "first")
              .replace("nanos", "second")
          else fixture
        withGraph(code) { graph =>
          val clock     = graph.typeDecl.nameExact(if (rename) "OtherClock" else "RenamedClock").head
          val widths    = graph.typeDecl.nameExact("PaddingWidths").head
          val carrier   = graph.typeDecl.nameExact("CarrierBox").head
          val clockKeys = fields(
            clock,
            Seq(
              "named_value"     -> None,
              "unnamed_padding" -> Some(3),
              "named_value"     -> None,
              "unnamed_padding" -> Some(0)
            )
          )
          val widthKeys =
            fields(widths, Seq("unnamed_padding" -> Some(1), "named_value" -> None, "unnamed_padding" -> Some(7)))
          val carrierKeys = fields(carrier, Seq("named_value" -> Some(2), "anonymous_record_carrier" -> None))
          val nestedOwner = owner(carrier.member.toList.sortBy(_.order).apply(1))
          val nested      = graph.typeDecl
            .filter(record =>
              record._taggedByOut.exists {
                case entry: Tag if entry.name == "clang.semanticType" =>
                  ujson.read(entry.value).obj.get("declaration_usr").exists(_.str == nestedOwner)
                case _ => false
              }
            )
            .toList
          nested.size shouldBe 1
          val expected = Map(
            owner(clock)   -> clockKeys,
            owner(widths)  -> widthKeys,
            owner(carrier) -> carrierKeys,
            nestedOwner    -> fields(nested.head, Seq("named_value" -> None))
          )
          val initializers = graph.call
            .filter(call =>
              call._taggedByOut.exists {
                case entry: Tag => entry.name == "clang.initializerFieldKeys"
                case _          => false
              }
            )
            .toList
          initializers.size shouldBe 4
          initializers.foreach { call =>
            ujson.read(tag(call, "clang.initializerFieldKeys")).arr.map(_.str).toSeq shouldBe expected(owner(call))
            call.argument.size shouldBe expected(owner(call)).size
          }
          initializers.find(call => owner(call) == owner(clock)).get.argument.code.toList shouldBe List("11", "29")
        }
      }
    }
    "retain flexible member slots, implicit operands and independent nested array initialization" in {
      withGraph(
        "struct Flex{int length;int payload[];};struct Flex a={.length=1};" +
          "struct Flex b={.length=2,.payload={7,8}};int read(void){return a.length+b.payload[0];}"
      ) { graph =>
        val record    = graph.typeDecl.nameExact("Flex").head
        val keys      = fields(record, Seq("named_value" -> None, "named_value" -> None))
        val certified = graph.call
          .filter(call =>
            call._taggedByOut.exists {
              case entry: Tag => entry.name == "clang.initializerFieldKeys"
              case _          => false
            }
          )
          .toList
        certified.size shouldBe 2
        certified.foreach { call =>
          call.argument.size shouldBe 2
          ujson.read(tag(call, "clang.initializerFieldKeys")).arr.map(_.str).toSeq shouldBe keys
        }
        graph.call.nameExact("<operator>.arrayInitializer").size shouldBe 3
      }
    }
    "emit an unmodified native capture before malformed-payload controls" in {
      val selected = input(fixture)
      val document = ClangNative.load(library).parse(selected.command, selected.directory)
      val graph    = emit(selected, document)
      try graph.member.size shouldBe 10
      finally graph.close()
    }
    "reject wholly missing native field facts even without an initializer" in {
      val selected = input("struct Single {int member;};")
      val document = ClangNative.load(library).parse(selected.command, selected.directory)
      val names    = Seq("field_key", "field_role", "is_bitfield", "bit_width")
      document("nodes").obj.values.filter(_.obj.contains("field_key")).foreach { facts =>
        names.foreach(facts.obj.remove)
      }
      val failure = intercept[IllegalArgumentException] {
        val graph = emit(selected, document)
        graph.close()
      }
      failure.getMessage should include("field_key")
    }
    "reject malformed field and semantic-slot facts from actual native captures" in {
      val changes: Seq[(String, NativeValue => Unit)] = Seq(
        "missing key"      -> (facts => { facts.obj.remove("field_key"); () }),
        "duplicate key"    -> (facts => facts.obj("field_key") = NStr("c:@S@RenamedClock#field:0")),
        "carrier padding"  -> (facts => facts.obj("field_role") = NStr("unnamed_padding")),
        "false role"       -> (facts => facts.obj("field_role") = NBool(false)),
        "missing role"     -> (facts => { facts.obj.remove("field_role"); () }),
        "unknown role"     -> (facts => facts.obj("field_role") = NStr("unknown")),
        "named padding"    -> (facts => facts.obj("field_role") = NStr("unnamed_padding")),
        "false bitfield"   -> (facts => facts.obj("is_bitfield") = NBool(false)),
        "missing width"    -> (facts => { facts.obj.remove("bit_width"); () }),
        "negative width"   -> (facts => facts.obj("bit_width") = NNum(-1)),
        "fractional width" -> (facts => facts.obj("bit_width") = NNum(0.5)),
        "foreign key"      -> (facts => facts.obj("field_key") = NStr("foreign#field:0"))
      )
      for ((name, change) <- changes) {
        val selected   = input(fixture)
        val document   = ClangNative.load(library).parse(selected.command, selected.directory)
        val candidates = document("nodes").obj.values.toSeq.filter(_.obj.contains("field_key"))
        val field      =
          if (name.contains("width") || name == "false bitfield")
            candidates.find(_("field_role").str == "unnamed_padding").get
          else if (name == "carrier padding") candidates.find(_("field_role").str == "anonymous_record_carrier").get
          else if (name == "duplicate key") candidates.find(_("field_key").str == "c:@S@RenamedClock#field:2").get
          else candidates.find(_("field_role").str == "named_value").get
        change(field)
        intercept[IllegalArgumentException] {
          val graph = emit(selected, document)
          graph.close()
        }
      }
      for (mode <- Seq("missing", "swapped", "duplicate", "foreign", "padding", "count")) {
        val selected = input(fixture)
        val document = ClangNative.load(library).parse(selected.command, selected.directory)
        val entry    = document("nodes").obj.toSeq.find { case (_, facts) =>
          facts.obj.get("initializer_field_keys").exists(_.arr.size == 2) &&
          facts("semantic_type")("declaration_usr").str == "c:@S@RenamedClock"
        }.get
        val initializer = entry._2
        val keys        = initializer("initializer_field_keys").arr.toSeq
        if (mode == "missing") initializer.obj.remove("initializer_field_keys")
        else if (mode == "count") {
          def syntax(node: NativeValue): Seq[NativeValue] =
            Seq(node) ++ node.obj.get("inner").toSeq.flatMap(_.arr).flatMap(syntax)
          val expression = syntax(document("ast")).find(_.obj.get("id").contains(NStr(entry._1))).get
          expression("inner").arr.remove(1)
        } else
          initializer.obj("initializer_field_keys") = NArr.from(mode match {
            case "swapped"   => keys.reverse
            case "duplicate" => Seq(keys.head, keys.head)
            case "foreign"   => Seq(NStr("foreign#field:0"), keys.last)
            case "padding"   => Seq(NStr("c:@S@RenamedClock#field:1"), keys.last)
            case other       => throw new IllegalArgumentException(other)
          })
        val failure = intercept[IllegalArgumentException] {
          val graph = emit(selected, document)
          graph.close()
        }
        if (mode == "count") failure.getMessage should include("operand count differs")
      }
    }
  }
}
