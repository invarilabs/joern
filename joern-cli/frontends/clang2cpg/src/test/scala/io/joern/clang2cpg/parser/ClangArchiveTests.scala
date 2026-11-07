package io.joern.clang2cpg.parser
import io.joern.clang2cpg.parser.clang.ClangArchive
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.Files
import java.util.zip.ZipFile
class ClangArchiveTests extends AnyWordSpec with Matchers {
  "Completed evidence archives" should {
    "retain exact bytes and reject an existing archive without deleting evidence" in {
      val root = Files.createTempDirectory("archive-test-")
      val dir = Files.createDirectory(root.resolve("tu"))
      val bytes = Array[Byte](0,1,2,-1,5)
      Files.write(dir.resolve("clang.json"), bytes)
      val archive = ClangArchive.complete(dir)
      Files.exists(dir) shouldBe false
      val zip = new ZipFile(archive.toFile)
      try zip.getInputStream(zip.getEntry("clang.json")).readAllBytes().toSeq shouldBe bytes.toSeq
      finally zip.close()
      Files.createDirectory(dir)
      Files.write(dir.resolve("clang.json"), bytes)
      intercept[IllegalArgumentException] { ClangArchive.complete(dir) }
      Files.readAllBytes(dir.resolve("clang.json")).toSeq shouldBe bytes.toSeq
    }
  }
}
