package io.joern.clang2cpg.parser

import java.io.{InputStream, IOException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClangDocumentTests extends AnyWordSpec with Matchers {
  private class PartialInput(payload: String) extends InputStream {
    private val bytes = payload.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    private var position = 0
    var closed = false
    var bulkReads = 0
    override def read(): Int = {
      if (position == bytes.length) -1 else {val result = bytes(position) & 255; position += 1; result}
    }
    override def read(target: Array[Byte], offset: Int, length: Int): Int = {
      require(length <= 65536, "JSON reader requested an unbounded buffer")
      bulkReads += 1
      if (position == bytes.length) -1 else {
        val count = math.min(7, math.min(length, bytes.length - position))
        System.arraycopy(bytes, position, target, offset, count)
        position += count
        count
      }
    }
    override def readAllBytes(): Array[Byte] = throw new AssertionError("whole document read forbidden")
    override def readNBytes(length: Int): Array[Byte] = throw new AssertionError("whole document read forbidden")
    override def close(): Unit = closed = true
  }

  "Native JSON document reader" should {
    "parse bounded partial reads without materializing a whole input String" in {
      val expected = ujson.Obj("ast" -> ujson.Obj("kind" -> "TranslationUnitDecl"),
        "nodes" -> ujson.Arr.from((0 until 10000).map(value => ujson.Obj("value" -> value))))
      val input = new PartialInput(ujson.write(expected))
      ClangParser.readDocument(input) shouldBe expected
      input.bulkReads should be > 1
      input.closed shouldBe true
    }
    "close and reject malformed native documents" in {
      val input = new PartialInput("{\"ast\":")
      intercept[Exception](ClangParser.readDocument(input))
      input.closed shouldBe true
    }
    "close and propagate input failures" in {
      var closed = false
      val input = new InputStream {
        override def read(): Int = throw new IOException("capture unavailable")
        override def close(): Unit = closed = true
      }
      intercept[IOException](ClangParser.readDocument(input)).getMessage shouldBe "capture unavailable"
      closed shouldBe true
    }
  }
}
