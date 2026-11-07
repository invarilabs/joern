package io.joern.clang2cpg.parser.memory

import java.nio.file.{Files, Path}
import java.security.MessageDigest

/** An owned in-memory compiler snapshot. Never constructed from an AST dump. */
final case class NativeGraphEvidence(source: Path, document: NativeValue, command: Seq[String],
  tools: Map[String, String], toolAliases: Map[String, String]) {
  def validate(): Unit = document("inputs").obj.foreach { case (name, expected) =>
    val path = Path.of(name)
    require(Files.isRegularFile(path), "Native compiler input disappeared: " + name)
    val digest = MessageDigest.getInstance("SHA-256")
    val input = Files.newInputStream(path)
    try {
      val buffer = new Array[Byte](65536)
      var count = input.read(buffer)
      while (count != -1) { digest.update(buffer, 0, count); count = input.read(buffer) }
    } finally input.close()
    require(digest.digest().map(value => f"${value & 0xff}%02x").mkString == expected.str,
      "Native compiler input changed: " + name)
  }
}

object NativeGraphEvidence {
  def digest(path: Path): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val input = Files.newInputStream(path)
    try {
      val buffer = new Array[Byte](65536)
      var count = input.read(buffer)
      while (count != -1) { digest.update(buffer, 0, count); count = input.read(buffer) }
    } finally input.close()
    digest.digest().map(value => f"${value & 0xff}%02x").mkString
  }
}
