package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

class ClangDiskCaptureTests extends AnyWordSpec with Matchers {
  private def setup(copyPlugin: Boolean = false): (Path, ClangInput, Path, Config) = {
    val root = Files.createTempDirectory(Paths.get(sys.env("CLANG_CPG_TEST_ROOT")), "clang-disk-").toRealPath()
    val source = root.resolve("sample.c")
    Files.writeString(root.resolve("value.h"), "#define VALUE 1\n")
    Files.writeString(source, "#include \"value.h\"\nint f(void) {return VALUE;}\n")
    val original = Paths.get(sys.env("CLANG_CPG_PLUGIN")).toRealPath()
    val plugin = if (copyPlugin) Files.copy(original, root.resolve("plugin.so")) else original
    val input = ClangInput(source, Seq(sys.env("CLANG_CPG_COMPILER"), "-std=gnu11", source.toString), root)
    (root, input, plugin, Config(embedNativeEvidence = false))
  }

  "Native compilation staged on disk" should {
    "return a small capture without a document or receipt, then load identical native evidence" in {
      val (root, input, plugin, config) = setup()
      val captured = ClangParser.captureToDisk(input, plugin, root.resolve("evidence"), config)
      Files.isRegularFile(captured.directory.resolve("clang.json")) shouldBe true
      Files.exists(captured.directory.resolve("receipt.json")) shouldBe false
      captured.productIterator.exists(_.isInstanceOf[ujson.Value]) shouldBe false
      val evidence = ClangParser.load(captured)
      evidence.document shouldBe ClangParser.readDocument(Files.newInputStream(captured.directory.resolve("clang.json")))
      val receipt = ujson.read(Files.readString(captured.directory.resolve("receipt.json")))
      receipt("selected_command").arr.map(_.str).toSeq shouldBe input.command
      receipt("evidence_sha256").str shouldBe ClangParser.digest(captured.directory.resolve("clang.json"))
      receipt("inputs") shouldBe evidence.document("inputs")
      evidence.validate()
    }
    "reject source drift between compilation and document loading" in {
      val (root, input, plugin, config) = setup()
      val captured = ClangParser.captureToDisk(input, plugin, root.resolve("evidence"), config)
      Files.writeString(input.source, "int changed(void) {return 2;}\n")
      intercept[IllegalArgumentException](ClangParser.load(captured)).getMessage should include("changed")
      Files.exists(captured.directory.resolve("receipt.json")) shouldBe false
    }
    "reject header drift between compilation and document loading" in {
      val (root, input, plugin, config) = setup()
      val captured = ClangParser.captureToDisk(input, plugin, root.resolve("evidence"), config)
      Files.writeString(root.resolve("value.h"), "#define VALUE 2\n")
      intercept[IllegalArgumentException](ClangParser.load(captured)).getMessage should include("stale or missing")
      Files.exists(captured.directory.resolve("receipt.json")) shouldBe false
    }
    "reject protected tool drift between compilation and document loading" in {
      val (root, input, plugin, config) = setup(copyPlugin = true)
      val captured = ClangParser.captureToDisk(input, plugin, root.resolve("evidence"), config)
      Files.write(plugin, Array[Byte](0), StandardOpenOption.APPEND)
      intercept[IllegalArgumentException](ClangParser.load(captured)).getMessage should include("changed")
      Files.exists(captured.directory.resolve("receipt.json")) shouldBe false
    }
    "reject a protected invocation alias retargeted before loading" in {
      val (root, input, plugin, config) = setup()
      val alias = Files.createSymbolicLink(root.resolve("plugin-alias.so"), plugin)
      val captured = ClangParser.captureToDisk(input, alias, root.resolve("evidence"), config)
      Files.delete(alias)
      Files.createSymbolicLink(alias, input.source)
      intercept[IllegalArgumentException](ClangParser.load(captured)).getMessage should include("alias target changed")
      Files.exists(captured.directory.resolve("receipt.json")) shouldBe false
    }
    "reject command provenance drift before loading" in {
      val (root, input, plugin, config) = setup()
      val captured = ClangParser.captureToDisk(input, plugin, root.resolve("evidence"), config)
      Files.writeString(captured.directory.resolve("command.json"), "[]")
      intercept[IllegalArgumentException](ClangParser.load(captured)).getMessage should include("command changed")
      Files.exists(captured.directory.resolve("receipt.json")) shouldBe false
    }
    "reject changed native output without publishing a receipt" in {
      val (root, input, plugin, config) = setup()
      val captured = ClangParser.captureToDisk(input, plugin, root.resolve("evidence"), config)
      Files.writeString(captured.directory.resolve("clang.json"), "{\"ast\":")
      intercept[IllegalArgumentException](ClangParser.load(captured)).getMessage should include("artifact changed")
      Files.exists(captured.directory.resolve("receipt.json")) shouldBe false
    }
    "reject malformed producer JSON even when its capture hash matches" in {
      val (root, input, plugin, config) = setup()
      val captured = ClangParser.captureToDisk(input, plugin, root.resolve("evidence"), config)
      val document = captured.directory.resolve("clang.json")
      Files.writeString(document, "{\"ast\":")
      val malformed = captured.copy(artifactHashes = Seq(document -> ClangParser.digest(document)))
      intercept[Exception](ClangParser.load(malformed))
      Files.exists(captured.directory.resolve("receipt.json")) shouldBe false
    }
  }
}
