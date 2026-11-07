package io.joern.clang2cpg.parser.clang

import io.joern.clang2cpg.parser.memory.{NativeValue, NStr}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection.mutable

/** Lossless graph-scoped interning of compiler macro records; references preserve occurrence order. */
final class ClangMacroSites {
  private val values = mutable.LinkedHashMap.empty[String, String]

  def register(sites: Seq[NativeValue]): Seq[NativeValue] = sites.map { site =>
    require(site.objOpt.nonEmpty, "Compiler macro site must be an object")
    val value = NativeValue.write(site)
    val identity = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x").mkString
    values.get(identity).foreach(previous => require(previous == value, "Compiler macro site digest collision"))
    values.getOrElseUpdate(identity, value)
    NStr(identity)
  }

  def value(identity: String): String = values.getOrElse(identity,
    throw new IllegalArgumentException("Compiler macro site reference is missing: " + identity))
}
