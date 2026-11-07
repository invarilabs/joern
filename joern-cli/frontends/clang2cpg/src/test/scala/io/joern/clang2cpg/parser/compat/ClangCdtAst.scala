package io.joern.clang2cpg.parser.compat

import io.joern.clang2cpg.parser.ClangEvidence
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.ASTNode
import org.eclipse.cdt.internal.core.dom.parser.c.*
import org.eclipse.cdt.internal.core.parser.scanner.ILocationResolver
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.file.Files

/** Test-only compatibility experiment. No CDT parser or unresolved semantic defaults. */
object ClangCdtAst {
  private def unsupported(message: String): Nothing = throw new LinkageError("Clang/CDT compatibility: " + message)
  private def children(node: ujson.Value): Seq[ujson.Value] = node.obj.get("inner").map(_.arr.toSeq).getOrElse(Seq.empty)
  private def kind(node: ujson.Value): String = node("kind").str
  private def metadata(evidence: ClangEvidence, node: ujson.Value): ujson.Value =
    evidence.document("nodes").obj.getOrElse(node("id").str, unsupported("missing native node facts: " + kind(node)))
  private def selected(evidence: ClangEvidence): Seq[ujson.Value] = children(evidence.document("ast")).filter { node =>
    node.obj.get("id").flatMap(id => evidence.document("nodes").obj.get(id.str))
      .exists(_.obj.get("file").exists(_.str == evidence.source.toString))
  }
  def preflight(evidence: ClangEvidence): Unit = {
    val supported = Set("FunctionDecl", "ParmVarDecl", "VarDecl", "CompoundStmt", "ReturnStmt", "DeclStmt",
      "ImplicitCastExpr", "ParenExpr", "IntegerLiteral", "DeclRefExpr", "BinaryOperator")
    def visit(node: ujson.Value): Unit = {
      if (!supported(kind(node))) unsupported("unsupported compiler syntax " + kind(node))
      val fact = metadata(evidence, node)
      if (fact.obj.get("macro").exists(_.bool)) unsupported("macro expansion requires CDT-compatible expansion locations at " + fact.obj.get("line").map(_.num.toInt).getOrElse(0))
      children(node).foreach(visit)
    }
    selected(evidence).foreach(visit)
    if (selected(evidence).isEmpty) unsupported("no selected source declarations")
    if (evidence.document("macros").arr.nonEmpty) unsupported("macro expansion resolver is not implemented")
    if (evidence.document("macro_directives").arr.nonEmpty) unsupported("preprocessor directive adapter is not implemented")
    if (evidence.document("comments").arr.nonEmpty) unsupported("comment adapter is not implemented")
  }
  def translate(evidence: ClangEvidence): IASTTranslationUnit = {
    preflight(evidence)
    new Builder(evidence).translationUnit()
  }
  private class Builder(evidence: ClangEvidence) {
    private val types = new ClangCdtTypes(evidence)
    private val source = Files.readString(evidence.source)
    private def info(node: ujson.Value): ujson.Value = metadata(evidence, node)
    private def location(offset: Int, length: Int): IASTFileLocation = {
      if (offset < 0 || length < 0 || offset + length > source.length) unsupported("invalid source range")
      new IASTFileLocation {
        override def getFileName: String = evidence.source.toString
        override def getNodeOffset: Int = offset
        override def getNodeLength: Int = length
        override def getStartingLineNumber: Int = source.take(offset).count(_ == '\n') + 1
        override def getEndingLineNumber: Int = source.take(offset + length).count(_ == '\n') + 1
        override def getContextInclusionStatement: IASTPreprocessorIncludeStatement = null // Main file has no enclosing include.
        override def asFileLocation(): IASTFileLocation = this
      }
    }
    private def nodeLocation(node: ujson.Value): IASTFileLocation = {
      val fact = info(node)
      val offset = fact("offset").num.toInt
      location(offset, fact("end").num.toInt - offset)
    }
    private trait Located extends ASTNode {
      def origin: ujson.Value
      override def getRawSignature: String = info(origin)("code").str
      override def getFileLocation: IASTFileLocation = nodeLocation(origin)
      override def getNodeLocations: Array[IASTNodeLocation] = Array(nodeLocation(origin))
      override def getContainingFilename: String = evidence.source.toString
      override def isPartOfTranslationUnitFile: Boolean = true
      override def isPartOfSourceFile: Boolean = true
    }
    private def name(node: ujson.Value, declaration: String): CASTName = {
      val result = new CASTName(info(node)("name").str.toCharArray) with Located { val origin = node }
      result.setBinding(types.binding(declaration))
      result
    }
    private def specifier(node: ujson.Value, shape: ujson.Value): CASTSimpleDeclSpecifier = {
      val basic = types.value(shape) match {
        case value: IBasicType => value
        case other => unsupported("declarator requires a non-scalar type adapter: " + other.getClass.getName)
      }
      val result = new CASTSimpleDeclSpecifier with Located { val origin = node }
      result.setType(basic.getKind)
      result.setUnsigned(basic.isUnsigned)
      result.setSigned(basic.isSigned)
      result.setShort(basic.isShort)
      result.setLong(basic.isLong)
      result.setLongLong(basic.isLongLong)
      result
    }
    private def expression(node: ujson.Value): IASTExpression = kind(node) match {
      case "ImplicitCastExpr" | "ParenExpr" =>
        if (children(node).size != 1) unsupported("conversion requires exactly one child")
        expression(children(node).head)
      case "IntegerLiteral" =>
        new CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, node("value").str.toCharArray) with Located {
          val origin = node
          override def getExpressionType: IType = types.value(info(node)("semantic_type"))
        }
      case "DeclRefExpr" =>
        val declaration = node("referencedDecl")("id").str
        val targetName = new CASTName(node("referencedDecl")("name").str.toCharArray) with Located { val origin = node }
        targetName.setBinding(types.binding(declaration))
        new CASTIdExpression(targetName) with Located {
          val origin = node
          override def getExpressionType: IType = types.value(info(node)("semantic_type"))
        }
      case "BinaryOperator" =>
        val operators = Map("+" -> IASTBinaryExpression.op_plus, "-" -> IASTBinaryExpression.op_minus,
          "*" -> IASTBinaryExpression.op_multiply, "/" -> IASTBinaryExpression.op_divide,
          "=" -> IASTBinaryExpression.op_assign, "==" -> IASTBinaryExpression.op_equals,
          "<" -> IASTBinaryExpression.op_lessThan, ">" -> IASTBinaryExpression.op_greaterThan)
        val operation = operators.getOrElse(node("opcode").str, unsupported("binary operation " + node("opcode").str))
        if (children(node).size != 2) unsupported("binary expression requires two children")
        new CASTBinaryExpression(operation, expression(children(node).head), expression(children(node)(1))) with Located {
          val origin = node
          override def getExpressionType: IType = types.value(info(node)("semantic_type"))
        }
      case other => unsupported("expression " + other)
    }
    private def variable(node: ujson.Value): IASTSimpleDeclaration = {
      val declarator = new CASTDeclarator(name(node, node("id").str)) with Located { val origin = node }
      children(node) match {
        case Seq(value) => declarator.setInitializer(new CASTEqualsInitializer(expression(value)) with Located { val origin = node })
        case Seq() => () // Native AST proves this declaration has no explicit initializer.
        case _ => unsupported("variable has multiple initializer children")
      }
      val declaration = new CASTSimpleDeclaration(specifier(node, info(node)("semantic_type"))) with Located { val origin = node }
      declaration.addDeclarator(declarator)
      declaration
    }
    private def statement(node: ujson.Value): IASTStatement = kind(node) match {
      case "CompoundStmt" =>
        val block = new CASTCompoundStatement with Located { val origin = node }
        children(node).foreach(child => block.addStatement(statement(child)))
        block
      case "ReturnStmt" =>
        if (children(node).size != 1) unsupported("only explicit-valued returns are implemented")
        new CASTReturnStatement(expression(children(node).head)) with Located { val origin = node }
      case "DeclStmt" =>
        if (children(node).size != 1 || kind(children(node).head) != "VarDecl") unsupported("compound declaration adapter is missing")
        new CASTDeclarationStatement(variable(children(node).head)) with Located { val origin = node }
      case _ => new CASTExpressionStatement(expression(node)) with Located { val origin = node }
    }
    private def declaration(node: ujson.Value): IASTDeclaration = kind(node) match {
      case "VarDecl" => variable(node)
      case "FunctionDecl" =>
        val function = new CASTFunctionDeclarator(name(node, node("id").str)) with Located { val origin = node }
        val shape = info(node)("semantic_type")
        function.setVarArgs(shape("variadic").bool)
        children(node).filter(kind(_) == "ParmVarDecl").foreach { parameter =>
          val value = new CASTDeclarator(name(parameter, parameter("id").str)) with Located { val origin = parameter }
          function.addParameterDeclaration(new CASTParameterDeclaration(specifier(parameter, info(parameter)("semantic_type")), value) with Located { val origin = parameter })
        }
        val bodies = children(node).filter(kind(_) == "CompoundStmt")
        if (bodies.size != 1) unsupported("function prototype compatibility is not implemented")
        new CASTFunctionDefinition(specifier(node, shape("result")), function, statement(bodies.head)) with Located { val origin = node }
      case other => unsupported("declaration " + other)
    }
    def translationUnit(): IASTTranslationUnit = {
      val unit = new CASTTranslationUnit {
        override def getRawSignature: String = source
        override def getReferences(binding: IBinding): Array[IASTName] = unsupported("getReferences requires a compiler reference index")
      }
      val resolver = Proxy.newProxyInstance(classOf[ILocationResolver].getClassLoader, Array(classOf[ILocationResolver]), new InvocationHandler {
        override def invoke(proxy: Any, method: Method, arguments: Array[Object]): Object = method.getName match {
          case "setRootNode" => null // CDT setter has void return; unit identity is already owned here.
          case "getTranslationUnitPath" | "getContainingFilePath" => evidence.source.toString
          case "getLocations" => Array[IASTNodeLocation](location(arguments(0).asInstanceOf[Integer].intValue, arguments(1).asInstanceOf[Integer].intValue))
          case "getMappedFileLocation" => location(arguments(0).asInstanceOf[Integer].intValue, arguments(1).asInstanceOf[Integer].intValue)
          case "flattenLocations" =>
            val locations = arguments(0).asInstanceOf[Array[IASTNodeLocation]]
            if (locations.isEmpty) unsupported("empty source locations")
            val first = locations.head.asFileLocation; val last = locations.last.asFileLocation
            location(first.getNodeOffset, last.getNodeOffset + last.getNodeLength - first.getNodeOffset)
          case "isPartOfTranslationUnitFile" | "isPartOfSourceFile" => java.lang.Boolean.TRUE
          case "getUnpreprocessedSignature" =>
            val loc = arguments(0).asInstanceOf[IASTFileLocation]
            source.substring(loc.getNodeOffset, loc.getNodeOffset + loc.getNodeLength).toCharArray
          case "getIncludeDirectives" => Array.empty[IASTPreprocessorIncludeStatement] // Preflight proves no directives.
          case "getAllPreprocessorStatements" => Array.empty[IASTPreprocessorStatement]
          case "getMacroExpansions" => Array.empty[IASTPreprocessorMacroExpansion]
          case "getComments" => Array.empty[IASTComment]
          case other => unsupported("location resolver method " + other)
        }
      }).asInstanceOf[ILocationResolver]
      unit.setLocationResolver(resolver)
      unit.setOffsetAndLength(0, source.length)
      selected(evidence).foreach(node => unit.addDeclaration(declaration(node)))
      unit
    }
  }
}
