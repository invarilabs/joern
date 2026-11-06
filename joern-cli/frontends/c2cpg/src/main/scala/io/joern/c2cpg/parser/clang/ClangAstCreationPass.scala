package io.joern.c2cpg.parser.clang

import io.joern.c2cpg.parser.ClangEvidence
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.CpgPass
import java.nio.file.Paths
import scala.collection.mutable

/** Native Clang syntax to CPG. Unsupported executable shapes abort graph creation. */
class ClangAstCreationPass(graph: Cpg, evidence: ClangEvidence) extends CpgPass(graph) {
  private val document                  = evidence.document
  private val metadata                  = document("nodes").obj
  private val declarations              = mutable.LinkedHashMap.empty[String, ujson.Value]
  private val bindings                  = mutable.LinkedHashMap.empty[String, NewNode]
  private val methods                   = mutable.LinkedHashMap.empty[String, NewMethod]
  private val namespaces                = mutable.LinkedHashMap.empty[String, NewNamespaceBlock]
  private val astParents                = mutable.Map.empty[NewNode, NewNode]
  private val methodBodies              = mutable.Map.empty[NewMethod, NewBlock]
  private val pendingReferences         = mutable.ArrayBuffer.empty[(NewNode, String)]
  private val pendingGotos              = mutable.ArrayBuffer.empty[(NewNode, String)]
  private val labels                    = mutable.LinkedHashMap.empty[String, NewJumpTarget]
  private var builder: DiffGraphBuilder = null
  private val added = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[NewNode, java.lang.Boolean]())
  private def add(node: NewNode): Unit = {
    if (added.add(node)) builder.addNode(node)
  }

  private def children(node: ujson.Value): Seq[ujson.Value] =
    node.obj.get("inner").map(_.arr.toSeq).getOrElse(Seq.empty)
  private def kind(node: ujson.Value): String      = node.obj.get("kind").map(_.str).getOrElse("")
  private def identity(node: ujson.Value): String  = node("id").str
  private def info(node: ujson.Value): ujson.Value =
    metadata.getOrElse(
      identity(node),
      throw new IllegalArgumentException(s"Clang node lacks native evidence: ${kind(node)}")
    )
  private def text(node: ujson.Value): String          = info(node).obj.get("code").map(_.str).getOrElse("")
  private def canonicalType(node: ujson.Value): String = {
    val value = info(node)("type").str
    require(value.nonEmpty && value != "ANY", s"Clang node has unresolved type: ${kind(node)} ${text(node)}")
    value
  }
  val sourceTypeBindings                = mutable.LinkedHashMap.empty[String, String]
  private lazy val canonicalRecordTypes = declarations.values
    .filter(node => Set("RecordDecl", "EnumDecl")(kind(node)))
    .map(canonicalType)
    .toSet
  private def valueType(node: ujson.Value): String = {
    val canonical = canonicalType(node)
    if (Set("RecordDecl", "EnumDecl", "TypedefDecl")(kind(node))) return canonical
    val spelling = node.obj
      .get("type")
      .flatMap(_.obj.get("qualType"))
      .map(_.str)
      .getOrElse(throw new IllegalArgumentException(s"Clang node lacks source type spelling: ${kind(node)}"))
    require(spelling.nonEmpty && spelling != "ANY", "Clang source type spelling is unresolved")
    if (canonicalRecordTypes(canonical)) {
      sourceTypeBindings
        .get(spelling)
        .foreach(previous =>
          require(previous == canonical, s"Clang source type spelling is ambiguous across declarations: $spelling")
        )
      sourceTypeBindings(spelling) = canonical
    }
    spelling
  }
  private def declarationUsr(node: ujson.Value): String = {
    val value = info(node).obj
      .get("usr")
      .map(_.str)
      .getOrElse(
        throw new IllegalArgumentException(s"Clang declaration lacks compiler USR: ${kind(node)} ${text(node)}")
      )
    require(value.nonEmpty, "Clang declaration has an empty compiler USR")
    value
  }
  private def canonical(node: ujson.Value): String      = info(node)("canonical").str
  private def source(node: ujson.Value): Option[String] = info(node).obj.get("file").map(_.str).filter(_.nonEmpty)
  private def inMain(node: ujson.Value): Boolean        =
    source(node).exists(name => Paths.get(name).toAbsolutePath.normalize() == evidence.source)
  private def graphFilename(name: String): String = {
    val path = Paths.get(name).toAbsolutePath.normalize()
    if (path.startsWith(evidence.source.getParent)) evidence.source.getParent.relativize(path).toString
    else path.toString
  }
  private def filename(node: ujson.Value): String = source(node) match {
    case Some(name) => graphFilename(name)
    case None       => "<compiler>"
  }
  private def index(node: ujson.Value): Unit = {
    if (kind(node).endsWith("Decl") && node.obj.contains("id"))
      declarations.get(identity(node)) match {
        case Some(previous) => require(previous == node, s"Conflicting Clang declaration ${identity(node)}")
        case None           => declarations(identity(node)) = node
      }
    children(node).foreach(index)
  }
  private def ast(parent: NewNode, child: NewNode, order: Int): Unit = {
    child match {
      case node: AstNodeNew => node.order = order
      case _                => throw new IllegalArgumentException(s"Non-AST child ${child.label}")
    }
    add(child)
    astParents(child) = parent
    builder.addEdge(parent, child, EdgeTypes.AST)
  }
  private def position(node: AstNodeNew, original: ujson.Value): Unit = {
    val origin = info(original)
    origin.obj.get("line").foreach(value => node.lineNumber = Some(value.num.toInt))
    origin.obj.get("column").foreach(value => node.columnNumber = Some(value.num.toInt))
    origin.obj.get("offset").foreach(value => node.offset = Some(value.num.toInt))
    origin.obj.get("end").foreach(value => node.offsetEnd = Some(value.num.toInt))
  }
  private def tag(node: NewNode, name: String, value: String): Unit = {
    val annotation = NewTag().name(name).value(value)
    add(annotation)
    builder.addEdge(node, annotation, EdgeTypes.TAGGED_BY)
  }
  private def semanticAnnotations(node: NewNode, original: ujson.Value): Unit = {
    info(original).obj.get("semantic_type").foreach(value => tag(node, "clang.semanticType", ujson.write(value)))
    info(original).obj.get("usr").foreach(value => tag(node, "clang.declarationIdentity", value.str))
    info(original).obj.get("constant_value").foreach(value => tag(node, "clang.constantValue", value.str))
    info(original).obj.get("string_literal").foreach(value => tag(node, "clang.stringLiteral", ujson.write(value)))
    info(original).obj.get("storage_duration").foreach(value => tag(node, "clang.storageDuration", value.str))
    info(original).obj.get("initializer_kind").foreach(value => tag(node, "clang.initializerKind", value.str))
  }
  private def annotate(node: AstNodeNew, original: ujson.Value): Unit = {
    position(node, original)
    tag(node, "clang.kind", kind(original))
    semanticAnnotations(node, original)
    info(original).obj.get("type").foreach(value => tag(node, "clang.canonicalType", value.str))
    original.obj.get("value").foreach(value => tag(node, "clang.value", value.strOpt.getOrElse(ujson.write(value))))
    original.obj.get("castKind").foreach { value =>
      tag(node, "clang.castKind", value.str)
      tag(node, "clang.writtenCastKind", value.str)
    }
  }
  private def namespace(file: String): NewNamespaceBlock = namespaces.getOrElseUpdate(
    file, {
      val unit = NewFile().name(file).order(0)
      add(unit)
      val namespace = NewNamespaceBlock().name("<global>").fullName(file + ":<global>").filename(file)
      ast(unit, namespace, 1)
      namespace
    }
  )
  private def ref(node: NewNode, declaration: String): Unit = pendingReferences += node -> declaration
  private def argument(parent: NewNode, child: ExpressionNew, index: Int): Unit = {
    child.argumentIndex = index
    ast(parent, child, index)
    builder.addEdge(parent, child, EdgeTypes.ARGUMENT)
  }
  private def declarationName(node: ujson.Value): String = {
    val name = info(node).obj.get("name").map(_.str).getOrElse("")
    if (name.nonEmpty) name else "<anonymous>"
  }
  private def registerTypes(): Unit = {
    val groups = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[ujson.Value]]
    declarations.values.filter(node => Set("RecordDecl", "EnumDecl")(kind(node))).foreach { node =>
      groups.getOrElseUpdate(canonicalType(node), mutable.ArrayBuffer.empty) += node
    }
    groups.foreach { case (fullName, alternatives) =>
      val definitions = alternatives.filter(node =>
        node.obj.get("completeDefinition").exists(_.bool) ||
          (kind(node) == "EnumDecl" && children(node).exists(kind(_) == "EnumConstantDecl"))
      )
      require(definitions.size <= 1, s"Multiple Clang definitions for canonical type $fullName")
      val selected    = definitions.headOption.getOrElse(alternatives.head)
      val declaration = NewTypeDecl()
        .name(declarationName(selected))
        .fullName(fullName)
        .filename(filename(selected))
        .code(text(selected))
        .isExternal(!inMain(selected))
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(filename(selected) + ":<global>")
      annotate(declaration, selected)
      val owner = namespace(filename(selected))
      ast(owner, declaration, owner.order + 1)
      alternatives.foreach(node => bindings(identity(node)) = declaration)
      children(selected).filter(node => Set("FieldDecl", "EnumConstantDecl")(kind(node))).zipWithIndex.foreach {
        case (field, index) =>
          val name          = info(field).obj.get("name").map(_.str).getOrElse("")
          val effectiveName = if (name.nonEmpty) name else "<anonymous-field>:" + info(field)("offset").num.toLong
          val member        = NewMember().name(effectiveName).code(text(field)).typeFullName(valueType(field))
          annotate(member, field)
          ast(declaration, member, index + 1)
          bindings(identity(field)) = member
      }
    }
    declarations.values.filter(kind(_) == "TypedefDecl").foreach { node =>
      val name     = declarationName(node)
      val fullName = filename(node) + ":typedef:" + name + ":" + info(node).obj
        .get("offset")
        .map(_.num.toLong.toString)
        .getOrElse("compiler")
      val alias = NewTypeDecl()
        .name(name)
        .fullName(fullName)
        .filename(filename(node))
        .code(text(node))
        .aliasTypeFullName(valueType(node))
        .isExternal(!inMain(node))
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(filename(node) + ":<global>")
      annotate(alias, node)
      ast(namespace(filename(node)), alias, 1)
      bindings(identity(node)) = alias
    }
  }
  private var translationUnitScope: Option[(NewMethod, NewBlock)] = None
  private def globalScope(): (NewMethod, NewBlock)                = translationUnitScope.getOrElse {
    val file   = evidence.source.getFileName.toString
    val owner  = namespace(file)
    val method = NewMethod()
      .name("<global>")
      .fullName(file + ":<global>:clang")
      .signature("void()")
      .filename(file)
      .code("<global>")
      .isExternal(false)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(owner.fullName)
    ast(owner, method, 1)
    val block = NewBlock().code("<global>").typeFullName("void")
    ast(method, block, 1)
    ast(method, NewMethodReturn().code("RET").typeFullName("void").evaluationStrategy(EvaluationStrategies.BY_VALUE), 2)
    val scope = method -> block
    translationUnitScope = Some(scope)
    scope
  }
  private def registerMethods(): Seq[(ujson.Value, NewMethod)] = {
    val groups = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[ujson.Value]]
    declarations.values.filter(kind(_) == "FunctionDecl").foreach { node =>
      groups.getOrElseUpdate(canonical(node), mutable.ArrayBuffer.empty) += node
    }
    val ordered = groups.toSeq.zipWithIndex
      .sortBy { case ((_, alternatives), index) =>
        val definitions = alternatives.filter(node => inMain(node) && children(node).exists(kind(_) == "CompoundStmt"))
        (
          if (definitions.nonEmpty) 0 else 1,
          definitions.headOption.map(node => info(node)("offset").num.toLong).getOrElse(index.toLong)
        )
      }
      .map(_._1)
    ordered.map { case (root, alternatives) =>
      val definitions = alternatives.filter(node => inMain(node) && children(node).exists(kind(_) == "CompoundStmt"))
      require(definitions.size <= 1, s"Multiple selected Clang function definitions for $root")
      val node = definitions.headOption.getOrElse(
        alternatives
          .find(identity(_) == root)
          .getOrElse(throw new IllegalArgumentException(s"Clang canonical function declaration is missing: $root"))
      )
      val name     = declarationName(node)
      val fullName = "clang:" + info(node)("usr").str + ":" + valueType(node)
      val method   = NewMethod()
        .name(name)
        .fullName(fullName)
        .signature(valueType(node))
        .filename(filename(node))
        .code(text(node))
        .isExternal(definitions.isEmpty)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(filename(node) + ":<global>")
      annotate(method, node)
      ast(namespace(filename(node)), method, 1)
      // Enter the real translation-unit scope alongside its first definition,
      // preserving c2cpg's source-method allocation convention.
      if (
        definitions.nonEmpty && declarations.values.exists(declaration =>
          kind(declaration) == "VarDecl" && inMain(declaration) && info(declaration)("file_scope").bool
        )
      ) globalScope()
      alternatives.foreach { alternative =>
        methods(identity(alternative)) = method
        bindings(identity(alternative)) = method
      }
      methods(root) = method
      children(node).filter(kind(_) == "ParmVarDecl").zipWithIndex.foreach { case (parameter, index) =>
        val originalName = info(parameter).obj.get("name").map(_.str).getOrElse("")
        val name         = if (originalName.nonEmpty) originalName else "<unnamed-parameter>:" + index
        val value        = NewMethodParameterIn()
          .name(name)
          .code(text(parameter))
          .typeFullName(valueType(parameter))
          .index(index + 1)
          .evaluationStrategy(EvaluationStrategies.BY_VALUE)
          .isVariadic(false)
        annotate(value, parameter)
        ast(method, value, index + 1)
        bindings(identity(parameter)) = value
      }
      val result = NewMethodReturn()
        .code("RET")
        .typeFullName(info(node)("result_spelling").str)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      tag(result, "clang.semanticType", ujson.write(info(node)("semantic_type")("result")))
      ast(method, result, children(node).count(kind(_) == "ParmVarDecl") + 2)
      node -> method
    }
  }
  private def call(original: ujson.Value, name: String, operands: Seq[ExpressionNew]): NewCall = {
    val node = NewCall()
      .name(name)
      .methodFullName(name)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(text(original))
      .typeFullName(valueType(original))
    annotate(node, original)
    add(node)
    operands.zipWithIndex.foreach { case (operand, index) => argument(node, operand, index + 1) }
    node
  }
  private val binaryOperators = Map(
    "="   -> Operators.assignment,
    "+="  -> Operators.assignmentPlus,
    "-="  -> Operators.assignmentMinus,
    "*="  -> Operators.assignmentMultiplication,
    "/="  -> Operators.assignmentDivision,
    "%="  -> Operators.assignmentModulo,
    "&="  -> Operators.assignmentAnd,
    "|="  -> Operators.assignmentOr,
    "^="  -> Operators.assignmentXor,
    "<<=" -> Operators.assignmentShiftLeft,
    ">>=" -> Operators.assignmentArithmeticShiftRight,
    "+"   -> Operators.addition,
    "-"   -> Operators.subtraction,
    "*"   -> Operators.multiplication,
    "/"   -> Operators.division,
    "%"   -> Operators.modulo,
    "=="  -> Operators.equals,
    "!="  -> Operators.notEquals,
    "<"   -> Operators.lessThan,
    ">"   -> Operators.greaterThan,
    "<="  -> Operators.lessEqualsThan,
    ">="  -> Operators.greaterEqualsThan,
    "&&"  -> Operators.logicalAnd,
    "||"  -> Operators.logicalOr,
    "&"   -> Operators.and,
    "|"   -> Operators.or,
    "^"   -> Operators.xor,
    "<<"  -> Operators.shiftLeft,
    ">>"  -> Operators.arithmeticShiftRight,
    ","   -> "<operator>.comma"
  )
  private def unwrap(original: ujson.Value): ujson.Value = {
    if (Set("ImplicitCastExpr", "ParenExpr")(kind(original))) {
      require(children(original).size == 1, "Clang transparent wrapper lacks one operand")
      unwrap(children(original).head)
    } else original
  }
  private val activeMacros                                     = mutable.Set.empty[Int]
  private def expression(original: ujson.Value): ExpressionNew = {
    val origin      = info(original)
    val offset      = origin.obj.get("offset").map(_.num.toInt)
    val invocations =
      if (kind(original) == "ImplicitCastExpr" || !origin.obj.get("macro").exists(_.bool)) Seq.empty
      else {
        document("macros").arr.toSeq.filter { macroSite =>
          macroSite.obj.get("file") == origin.obj.get("file") && macroSite.obj.get("offset") == origin.obj.get(
            "offset"
          ) &&
          macroSite("code").str.matches(java.util.regex.Pattern.quote(macroSite("name").str) + "(?s)(?:\\W.*)?")
        }
      }
    if (invocations.nonEmpty && !activeMacros(offset.get)) {
      require(invocations.size == 1, "Clang macro source invocation is ambiguous")
      val invocation = invocations.head
      val definition = invocation.obj.getOrElse(
        "definition",
        throw new IllegalArgumentException("Clang macro lacks compiler definition evidence")
      )
      val name           = invocation("name").str
      val definitionFile = graphFilename(definition("file").str)
      val fullName       = "clang:macro:" + definitionFile + ":" + definition(
        "offset"
      ).num.toLong + ":" + name + ":expansion:" + offset.get
      val method = NewMethod()
        .name(name)
        .fullName(fullName)
        .code(definition("code").str)
        .filename(definitionFile)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(definitionFile + ":<global>")
        .isExternal(true)
        .signature("")
      ast(namespace(definitionFile), method, 1)
      ast(
        method,
        NewMethodReturn()
          .code("RET")
          .typeFullName(valueType(original))
          .evaluationStrategy(EvaluationStrategies.BY_VALUE),
        1
      )
      val wrapper = NewCall()
        .name(name)
        .methodFullName(fullName)
        .code(invocation("code").str)
        .typeFullName(valueType(original))
        .dispatchType(DispatchTypes.INLINED)
      annotate(wrapper, original)
      tag(wrapper, "clang.macroInvocation", ujson.write(invocation))
      add(wrapper)
      val block = NewBlock().code("<empty>").typeFullName(valueType(original))
      ast(wrapper, block, 1)
      activeMacros += offset.get
      val expanded = try expressionSyntax(original)
      finally activeMacros -= offset.get
      ast(block, expanded, 1)
      wrapper
    } else expressionSyntax(original)
  }
  private def expressionSyntax(original: ujson.Value): ExpressionNew = {
    val operands = children(original)
    kind(original) match {
      case "ParenExpr" | "ConstantExpr" =>
        require(operands.size == 1, "Clang expression wrapper requires one child")
        val result = expression(operands.head)
        original.obj.get("value").foreach(value => tag(result, "clang.constantValue", value.str))
        result
      case "ImplicitCastExpr" =>
        require(operands.size == 1, "Clang implicit conversion requires one child")
        val result     = expression(operands.head)
        val conversion =
          ujson.Obj(
            "kind"        -> original("castKind"),
            "from"        -> valueType(operands.head),
            "to"          -> valueType(original),
            "source_type" -> info(operands.head)("semantic_type"),
            "target_type" -> info(original)("semantic_type")
          )
        // Compiler conversions are not written source casts. Keep their complete
        // typed evidence on the operand without inventing a source operation.
        tag(result, "clang.implicitConversion", ujson.write(conversion))
        tag(result, "clang.castKind", original("castKind").str)
        result
      case "CStyleCastExpr" =>
        require(operands.size == 1, "Clang cast requires one child")
        val target = NewTypeRef().code(valueType(original)).typeFullName(valueType(original))
        add(target)
        call(original, Operators.cast, Seq(target, expression(operands.head)))
      case "IntegerLiteral" | "FloatingLiteral" | "CharacterLiteral" | "StringLiteral" =>
        val spelling = info(original).obj
          .get("spelling_code")
          .map(_.str)
          .getOrElse(throw new IllegalArgumentException("Clang literal lacks physical spelling evidence"))
        val literal = NewLiteral().code(spelling).typeFullName(valueType(original))
        annotate(literal, original)
        add(literal)
        literal
      case "DeclRefExpr" =>
        val reference   = original("referencedDecl")
        val declaration = reference("id").str
        val node        = if (reference("kind").str == "FunctionDecl") {
          val target =
            methods.getOrElse(declaration, throw new IllegalArgumentException(s"Missing Clang function $declaration"))
          NewMethodRef().code(text(original)).typeFullName(valueType(original)).methodFullName(target.fullName)
        } else NewIdentifier().name(reference("name").str).code(text(original)).typeFullName(valueType(original))
        annotate(node, original)
        add(node)
        ref(node, declaration)
        node
      case "BinaryOperator" | "CompoundAssignOperator" =>
        require(operands.size == 2, "Clang binary expression requires two operands")
        val opcode = original("opcode").str
        call(
          original,
          binaryOperators
            .getOrElse(opcode, throw new IllegalArgumentException(s"Unsupported Clang binary operator $opcode")),
          operands.map(expression)
        )
      case "UnaryOperator" =>
        require(operands.size == 1, "Clang unary expression requires one operand")
        val opcode   = original("opcode").str
        val operator = opcode match {
          case "!"   => Operators.logicalNot
          case "~"   => Operators.not
          case "-"   => Operators.minus
          case "+"   => Operators.plus
          case "&"   => Operators.addressOf
          case "*"   => Operators.indirection
          case "++"  => if (original("isPostfix").bool) Operators.postIncrement else Operators.preIncrement
          case "--"  => if (original("isPostfix").bool) Operators.postDecrement else Operators.preDecrement
          case other => throw new IllegalArgumentException(s"Unsupported Clang unary operator $other")
        }
        call(original, operator, operands.map(expression))
      case "MemberExpr" =>
        require(operands.size == 1, "Clang member requires one owner")
        val declaration = original("referencedMemberDecl").str
        val field       =
          declarations.getOrElse(declaration, throw new IllegalArgumentException(s"Missing Clang field $declaration"))
        val name   = bindings(declaration).asInstanceOf[NewMember].name
        val member = NewFieldIdentifier().canonicalName(name).code(name)
        annotate(member, field)
        add(member)
        ref(member, declaration)
        call(
          original,
          if (original("isArrow").bool) Operators.indirectFieldAccess else Operators.fieldAccess,
          Seq(expression(operands.head), member)
        )
      case "ArraySubscriptExpr" =>
        require(operands.size == 2, "Clang array selection requires two operands")
        call(original, Operators.indexAccess, operands.map(expression))
      case "ConditionalOperator" =>
        require(operands.size == 3, "Clang conditional requires three operands")
        call(original, Operators.conditional, operands.map(expression))
      case "UnaryExprOrTypeTraitExpr" =>
        require(original("name").str == "sizeof", "Only sizeof type-trait is supported by the Clang prototype")
        val arguments = original.obj.get("argType") match {
          case Some(_) =>
            require(operands.isEmpty, "Clang sizeof type operand has unexpected executable children")
            val operand = original("argType").obj
              .get("qualType")
              .map(_.str)
              .getOrElse(throw new IllegalArgumentException("Clang sizeof lacks its compiler operand spelling"))
            Seq(NewTypeRef().code(operand).typeFullName(operand))
          case None =>
            require(operands.size == 1, "Clang sizeof requires one unevaluated expression"); operands.map(expression)
        }
        call(original, Operators.sizeOf, arguments)
      case "CallExpr" =>
        require(operands.nonEmpty, "Clang call lacks a callee")
        val callee = unwrap(operands.head)
        val direct = kind(callee) == "DeclRefExpr" && callee("referencedDecl")("kind").str == "FunctionDecl"
        if (direct) {
          val target = methods(callee("referencedDecl")("id").str)
          val result = call(original, target.name, operands.tail.map(expression))
          result.methodFullName = target.fullName
          result.signature = target.signature
          result
        } else {
          val result = call(original, "<operator>.pointerCall", operands.tail.map(expression))
          result.dispatchType = DispatchTypes.DYNAMIC_DISPATCH
          val receiver = expression(operands.head)
          argument(result, receiver, -1)
          builder.addEdge(result, receiver, EdgeTypes.RECEIVER)
          result
        }
      case "InitListExpr" =>
        require(
          !original.obj.contains("array_filler"),
          "Clang array filler normalization is not implemented; explicit support is required"
        )
        call(original, "<operator>.arrayInitializer", operands.map(expression))
      case "ImplicitValueInitExpr" => call(original, "<operator>.implicitValueInit", Seq.empty)
      case "CompoundLiteralExpr"   => call(original, "<operator>.compoundLiteral", operands.map(expression))
      case other                   =>
        throw new IllegalArgumentException(
          s"Unsupported Clang expression $other in ${filename(original)}:${info(original).obj.get("line")}: ${text(original)}"
        )
    }
  }
  private def declarationMacro(original: ujson.Value, local: NewLocal): Unit = {
    val origin     = info(original)
    val begin      = origin("offset").num.toInt
    val end        = origin("end").num.toInt
    val expansions = document("macros").arr.toSeq.filter { expansion =>
      expansion.obj.get("file") == origin.obj.get("file") &&
      expansion("offset").num.toInt >= begin && expansion("end").num.toInt <= end &&
      expansion("code").str.matches(java.util.regex.Pattern.quote(expansion("name").str) + "(?s)(?:\\W.*)?")
    }
    // A sole declaration-prefix expansion owns the declaration's source span;
    // nested value expansions retain their own expression provenance instead.
    if (expansions.size == 1 && expansions.head("offset").num.toInt == begin) {
      val expansion = expansions.head
      tag(
        local,
        "clang.declarationMacro",
        ujson.write(
          ujson.Obj(
            "kind"        -> "VarDecl",
            "name"        -> expansion("name"),
            "begin"       -> begin,
            "end"         -> end,
            "source"      -> text(original),
            "declaration" -> declarationUsr(original)
          )
        )
      )
    }
  }
  private def statements(original: ujson.Value): Seq[AstNodeNew] = {
    val operands = children(original)
    kind(original) match {
      case "CompoundStmt" =>
        val block = NewBlock().code(text(original)).typeFullName("void")
        annotate(block, original)
        add(block)
        operands.flatMap(statements).zipWithIndex.foreach { case (child, index) => ast(block, child, index + 1) }
        Seq(block)
      case "DeclStmt" => operands.flatMap(statements)
      case "VarDecl"  =>
        val local = NewLocal().name(declarationName(original)).code(text(original)).typeFullName(valueType(original))
        annotate(local, original)
        add(local)
        bindings(identity(original)) = local
        val initializers = operands.filterNot(node => kind(node).endsWith("Attr"))
        require(initializers.size <= 1, s"Clang variable has unsupported declaration children: ${text(original)}")
        if (initializers.nonEmpty) declarationMacro(original, local)
        val assignments = initializers.map { initializer =>
          val target = NewIdentifier().name(local.name).code(local.name).typeFullName(local.typeFullName)
          position(target, original)
          tag(target, "clang.semanticType", ujson.write(info(original)("semantic_type")))
          add(target)
          ref(target, identity(original))
          val assignment = call(original, Operators.assignment, Seq(target, expression(initializer)))
          assignment.code = text(original)
          assignment
        }
        Seq(local) ++ assignments
      case "ReturnStmt" =>
        require(operands.size <= 1, "Clang return has multiple values")
        val result = NewReturn().code(text(original))
        annotate(result, original)
        add(result)
        operands.foreach(operand => argument(result, expression(operand), 1))
        Seq(result)
      case "IfStmt" | "WhileStmt" | "DoStmt" | "ForStmt" | "SwitchStmt" =>
        val category = kind(original) match {
          case "IfStmt"     => ControlStructureTypes.IF
          case "WhileStmt"  => ControlStructureTypes.WHILE
          case "DoStmt"     => ControlStructureTypes.DO
          case "ForStmt"    => ControlStructureTypes.FOR
          case "SwitchStmt" => ControlStructureTypes.SWITCH
        }
        val control =
          NewControlStructure().code(text(original)).controlStructureType(category).parserTypeName(kind(original))
        annotate(control, original)
        add(control)
        val conditionIndex = kind(original) match {
          case "ForStmt" => 2
          case "DoStmt"  => 1
          case _         => 0
        }
        operands.zipWithIndex.foreach { case (child, index) =>
          if (kind(child).nonEmpty) {
            val roots = statements(child)
            roots.zipWithIndex.foreach { case (root, nestedIndex) =>
              ast(control, root, index + 1 + nestedIndex)
              if (index == conditionIndex) builder.addEdge(control, root, EdgeTypes.CONDITION)
              val role = (kind(original), index) match {
                case ("IfStmt", 1) | ("WhileStmt", 1) => Some(EdgeTypes.TRUE_BODY)
                case ("IfStmt", 2)                    => Some(EdgeTypes.FALSE_BODY)
                case ("DoStmt", 0)                    => Some(EdgeTypes.DO_BODY)
                case ("ForStmt", 0)                   => Some(EdgeTypes.FOR_INIT)
                case ("ForStmt", 3)                   => Some(EdgeTypes.FOR_UPDATE)
                case ("ForStmt", 4)                   => Some(EdgeTypes.FOR_BODY)
                case _                                => None
              }
              role.foreach(label => builder.addEdge(control, root, label))
            }
          }
        }
        Seq(control)
      case "GotoStmt" =>
        val jump = NewControlStructure()
          .controlStructureType(ControlStructureTypes.GOTO)
          .parserTypeName("GotoStmt")
          .code(text(original))
        annotate(jump, original)
        add(jump)
        pendingGotos += jump -> original("targetLabelDeclId").str
        Seq(jump)
      case "LabelStmt" =>
        val label = NewJumpTarget().name(original("name").str).code(original("name").str + ":")
        annotate(label, original)
        add(label)
        labels(original("declId").str) = label
        Seq(label) ++ operands.flatMap(statements)
      case "NullStmt"                                => Seq.empty // An empty C statement has no executable operation.
      case "TypedefDecl" | "RecordDecl" | "EnumDecl" => Seq.empty // Already registered as non-executable declarations.
      case _ if metadata.get(identity(original)).exists(_.obj.contains("type")) => Seq(expression(original))
      case other => throw new IllegalArgumentException(s"Unsupported Clang statement $other: ${text(original)}")
    }
  }
  override def run(diffGraph: DiffGraphBuilder): Unit = {
    builder = diffGraph
    index(document("ast"))
    declarations.values.foreach(info)
    namespace(evidence.source.getFileName.toString)
    registerTypes()
    val registeredMethods                       = registerMethods()
    val localDeclarations                       = mutable.LinkedHashSet.empty[String]
    def localVariables(node: ujson.Value): Unit = {
      if (kind(node) == "VarDecl") localDeclarations += identity(node)
      children(node).foreach(localVariables)
    }
    registeredMethods.foreach { case (original, _) =>
      children(original).filter(kind(_) == "CompoundStmt").foreach(localVariables)
    }
    declarations.values
      .filter(node => kind(node) == "VarDecl" && source(node).nonEmpty && info(node)("file_scope").bool)
      .foreach { node =>
        if (!localDeclarations(identity(node)) && !inMain(node)) {
          val member = NewMember().name(declarationName(node)).code(text(node)).typeFullName(valueType(node))
          annotate(member, node)
          ast(namespace(filename(node)), member, 1)
          bindings(identity(node)) = member
        }
      }
    val globalInitializers = declarations.values
      .filter(node => kind(node) == "VarDecl" && inMain(node) && info(node)("file_scope").bool)
      .toSeq
    if (globalInitializers.nonEmpty) {
      val (_, block) = globalScope()
      globalInitializers.zipWithIndex.foreach { case (original, index) =>
        val local = NewLocal().name(declarationName(original)).code(text(original)).typeFullName(valueType(original))
        annotate(local, original)
        ast(block, local, index * 2 + 1)
        bindings(identity(original)) = local
        val initializers = children(original).filterNot(node => kind(node).endsWith("Attr"))
        require(initializers.size <= 1, s"Unsupported Clang global initializer children: ${text(original)}")
        initializers.foreach { initializer =>
          val target = NewIdentifier()
            .name(declarationName(original))
            .code(declarationName(original))
            .typeFullName(valueType(original))
          position(target, original)
          tag(target, "clang.semanticType", ujson.write(info(original)("semantic_type")))
          add(target)
          ref(target, identity(original))
          ast(block, call(original, Operators.assignment, Seq(target, expression(initializer))), index * 2 + 2)
        }
      }
    }
    registeredMethods.foreach { case (original, method) =>
      if (!method.isExternal) {
        val origin           = info(original)
        val macroInvocations = document("macros").arr.filter { invocation =>
          invocation.obj.get("file") == origin.obj.get("file") && !invocation("macro").bool && invocation(
            "function_like"
          ).bool &&
          invocation("offset").num >= origin("offset").num && invocation("end").num <= origin("end").num
        }
        tag(method, "clang.functionMacroInvocations", ujson.write(ujson.Arr.from(macroInvocations)))
        val bodies = children(original).filter(kind(_) == "CompoundStmt")
        require(bodies.size == 1, s"Clang source method lacks one body: ${method.fullName}")
        val body = statements(bodies.head).head.asInstanceOf[NewBlock]
        ast(method, body, children(original).count(kind(_) == "ParmVarDecl") + 1)
        methodBodies(method) = body
      }
    }
    val imports                                        = mutable.Map.empty[(NewMethod, String), NewLocal]
    def sourceMethod(node: NewNode): Option[NewMethod] = astParents.get(node) match {
      case Some(method: NewMethod) if methodBodies.contains(method) => Some(method)
      case Some(parent)                                             => sourceMethod(parent)
      case None                                                     => None
    }
    pendingReferences.foreach { case (origin, target) =>
      val declared =
        bindings.getOrElse(target, throw new IllegalArgumentException(s"Clang REF target missing: $target"))
      val declaration =
        declarations.getOrElse(target, throw new IllegalArgumentException(s"Clang declaration missing: $target"))
      val imported =
        (kind(declaration) == "EnumConstantDecl" ||
          (kind(declaration) == "VarDecl" && info(declaration)("file_scope").bool)) && origin
          .isInstanceOf[NewIdentifier]
      val destination = sourceMethod(origin)
        .filter(_ => imported)
        .map { method =>
          imports.getOrElseUpdate(
            (method, canonical(declaration)), {
              val local = NewLocal()
                .name(declarationName(declaration))
                .code("<global> " + declarationName(declaration))
                .typeFullName(valueType(declaration))
              val syntax = origin.asInstanceOf[AstNodeNew]
              local.lineNumber = syntax.lineNumber
              local.columnNumber = syntax.columnNumber
              require(declarationUsr(declaration).nonEmpty, "Clang imported declaration identity is empty")
              semanticAnnotations(local, declaration)
              ast(methodBodies(method), local, 0)
              local
            }
          )
        }
        .getOrElse(declared)
      builder.addEdge(origin, destination, EdgeTypes.REF)
    }
    pendingGotos.foreach { case (origin, target) =>
      val destination =
        labels.getOrElse(target, throw new IllegalArgumentException(s"Clang goto target missing: $target"))
      val label = NewJumpLabel().name(destination.name).code(destination.name)
      ast(origin, label, 1)
      builder.addEdge(origin, label, EdgeTypes.JUMP_ARGUMENT)
    }
    tag(
      namespace(evidence.source.getFileName.toString),
      "clang.translationUnit",
      ujson.write(
        ujson.Obj(
          "schema_version" -> 1,
          "source"         -> evidence.source.toString,
          "inputs"         -> document("inputs"),
          "tools"   -> ujson.read(java.nio.file.Files.readString(evidence.directory.resolve("receipt.json")))("tools"),
          "command" -> ujson
            .read(java.nio.file.Files.readString(evidence.directory.resolve("receipt.json")))("selected_command"),
          "macro_sites"      -> document("macros"),
          "macro_directives" -> document("macro_directives")
        )
      )
    )
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
    tag(namespace(evidence.source.getFileName.toString), "clang.nativeEvidence", ujson.write(nativePayload))
  }
}
