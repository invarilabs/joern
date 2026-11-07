package io.joern.clang2cpg.parser.clang

import io.joern.clang2cpg.parser.ClangEvidence
import io.joern.clang2cpg.parser.memory.{NativeGraphEvidence, NativeValue, NObj, NArr, NStr, NNum, NBool}
import io.shiftleft.codepropertygraph.generated.Cpg
import java.nio.file.{Files, Path}
import scala.collection.immutable.ListMap

/** Explicit legacy JSON adapter. The JNI frontend uses ClangAstEmitter directly. */
class ClangAstCreationPass(graph: Cpg, evidence: ClangEvidence, graphRoot: Option[Path] = None,
  skipFunctionBodies: Boolean = false, includeFileContent: Boolean = false, embedNativeEvidence: Boolean = true,
  strings: ClangStringPool = new ClangStringPool, tags: ClangTagPool = new ClangTagPool)
  extends ClangAstEmitter(graph, ClangAstCreationPass.adapt(evidence, graphRoot, embedNativeEvidence),
    graphRoot, skipFunctionBodies, includeFileContent, embedNativeEvidence, strings, tags)

object ClangAstCreationPass {
  private def typed(value: ujson.Value): NativeValue = value match {
    case fields: ujson.Obj => NObj.from(fields.obj.iterator.map { case (key, entry) => key -> typed(entry) })
    case items: ujson.Arr => NArr.from(items.arr.iterator.map(typed))
    case value: ujson.Str => NStr(value.str)
    case value: ujson.Num => NNum(value.num)
    case value: ujson.Bool => NBool(value.bool)
    case _ => throw new IllegalArgumentException("Legacy Clang document contains an unsupported null native value")
  }

  private def adapt(evidence: ClangEvidence, graphRoot: Option[Path], embed: Boolean): NativeGraphEvidence = {
    val receipt = ujson.read(Files.readString(evidence.directory.resolve("receipt.json")))
    def strings(value: ujson.Value): Map[String, String] =
      ListMap.from(value.obj.iterator.map { case (key, entry) => key -> entry.str })
    val consumed = Set("ast", "nodes", "macros", "macro_directives", "inputs", "comments", "include_search", "direct_cpg_only")
    val document = NObj.from(evidence.document.obj.iterator.collect {
      case (key, value) if consumed(key) => key -> typed(value)
    })
    NativeGraphEvidence(evidence.source, document, receipt("selected_command").arr.map(_.str).toSeq,
      strings(receipt("tools")), strings(receipt("tool_aliases")),
      if (embed) Some(() => legacyPayload(evidence, graphRoot)) else None)
  }

  private def legacyPayload(evidence: ClangEvidence, graphRoot: Option[Path]): String = {
    val document = evidence.document
    val nativePayload = ujson.Obj(
      "schema_version"   -> 1,
      "source"           -> evidence.source.toString,
      "ast"              -> document("ast"),
      "compiler_graph"   -> document("compiler_graph"),
      "macro_names"      -> document("macro_names"),
      "predefines"       -> document("predefines"),
      "inputs"           -> document("inputs"),
      "tokens"           -> ujson.Str(java.nio.file.Files.readString(evidence.directory.resolve("clang.json.tokens"))),
      "receipt"          -> ujson.read(java.nio.file.Files.readString(evidence.directory.resolve("receipt.json"))),
      "compiler_command" -> ujson.read(java.nio.file.Files.readString(evidence.directory.resolve("command.json")))
    )
    val payload = if (graphRoot.isEmpty) ujson.write(nativePayload) else {
      val bytes = new java.io.ByteArrayOutputStream()
      val gzip = new java.util.zip.GZIPOutputStream(bytes)
      try gzip.write(ujson.write(nativePayload).getBytes(java.nio.charset.StandardCharsets.UTF_8)) finally gzip.close()
      ujson.write(ujson.Obj("schema_version" -> 2, "encoding" -> "gzip+base64", "payload" -> java.util.Base64.getEncoder.encodeToString(bytes.toByteArray)))
    }
    payload
  }
}
