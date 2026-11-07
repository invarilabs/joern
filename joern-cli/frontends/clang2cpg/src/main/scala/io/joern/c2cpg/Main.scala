package io.joern.c2cpg

/** Compatibility entrypoint expected by the unmodified stock c2cpg launcher. */
object Main {
  def main(args: Array[String]): Unit = io.joern.clang2cpg.Main.main(args)
}
