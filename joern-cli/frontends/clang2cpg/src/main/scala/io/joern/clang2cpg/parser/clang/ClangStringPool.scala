package io.joern.clang2cpg.parser.clang

import scala.collection.mutable

/** Owned by one graph's ordered AST assembly; never shares strings between graphs. */
final class ClangStringPool {
  private val values = mutable.HashMap.empty[String, String]

  /** Return the first equal string instance, preserving its complete contents. */
  def apply(value: String): String = {
    require(value != null, "Clang string pool cannot canonicalize null")
    values.getOrElseUpdate(value, value)
  }
}
