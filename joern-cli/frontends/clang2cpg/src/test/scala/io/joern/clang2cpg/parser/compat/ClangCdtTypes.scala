package io.joern.clang2cpg.parser.compat

import io.joern.clang2cpg.parser.ClangEvidence
import org.eclipse.cdt.core.dom.ILinkage
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.Linkage
import org.eclipse.cdt.internal.core.dom.parser.IntegralValue
import org.eclipse.cdt.internal.core.dom.parser.c.{CArrayType, CBasicType, CFunctionType, CPointerType, CQualifierType}
import scala.collection.mutable

/** Test-only compiler type/binding bridge. Unsupported semantic queries must escape CDT's NonFatal recovery. */
final class ClangCdtTypes(evidence: ClangEvidence) {
  private val metadata = evidence.document("nodes").obj
  private val declarations = mutable.LinkedHashMap.empty[String, ujson.Value]
  private val owners = mutable.LinkedHashMap.empty[String, Option[String]]
  private val bindings = mutable.LinkedHashMap.empty[String, IBinding]
  private val recordTypes = mutable.LinkedHashMap.empty[String, RecordBinding]
  private val enumTypes = mutable.LinkedHashMap.empty[String, EnumBinding]

  private def unsupported(message: String): Nothing =
    throw new LinkageError("Clang-to-CDT compatibility prototype: " + message)
  private def required(node: ujson.Value, name: String): ujson.Value =
    node.objOpt.getOrElse(unsupported("compiler field container is not an object")).getOrElse(name, unsupported("missing compiler field " + name))
  private def text(node: ujson.Value, name: String): String = required(node, name) match {
    case ujson.Str(result) => result
    case _ => unsupported("compiler field is not a string: " + name)
  }
  private def children(node: ujson.Value): Seq[ujson.Value] = node.obj.get("inner").map(_.arr.toSeq).getOrElse(Seq.empty)
  private def nodeKind(node: ujson.Value): String = node.obj.get("kind").map(_.str).getOrElse("")
  private def facts(identifier: String): ujson.Value = metadata.getOrElse(identifier, unsupported("missing metadata for " + identifier))
  private def syntax(identifier: String): ujson.Value = declarations.getOrElse(identifier, unsupported("missing declaration syntax " + identifier))

  private def index(node: ujson.Value, owner: Option[String]): Unit = {
    val kind = nodeKind(node)
    val declaration = kind.endsWith("Decl") && node.obj.contains("id")
    val identifier = node.obj.get("id").map(_.str)
    if (declaration) {
      val key = identifier.get
      declarations.get(key) match {
        case Some(previous) if children(node).size > children(previous).size => declarations(key) = node
        case None => declarations(key) = node
        case _ =>
      }
      owners.get(key).foreach(previous => if (previous != owner) unsupported("declaration has conflicting owners: " + key))
      owners(key) = owner
    }
    val next = if (Set("FunctionDecl", "RecordDecl", "EnumDecl")(kind)) identifier else owner
    children(node).foreach(child => index(child, next))
  }
  index(evidence.document("ast"), None)

  private def declarationFor(shape: ujson.Value, kind: String): String = {
    val identity = text(shape, "declaration_usr")
    val matches = declarations.iterator.collect {
      case (identifier, node) if nodeKind(node) == kind && metadata.get(identifier).exists { entry =>
        entry.obj.get("semantic_type").exists(value => value.obj.get("declaration_usr").exists(_.str == identity) &&
          value.obj.get("declaration_identity") == shape.obj.get("declaration_identity"))
      } => identifier
    }.toSeq
    if (matches.isEmpty) unsupported("missing compiler declaration for " + identity)
    val canonical = matches.groupBy(identifier => text(facts(identifier), "canonical"))
    if (canonical.size != 1) unsupported("ambiguous compiler declaration identity " + identity)
    matches.find(identifier => children(syntax(identifier)).nonEmpty).getOrElse(matches.head)
  }

  /** Uses compiler structural constructors; never parses or compares display spellings. */
  def value(shape: ujson.Value): IType = {
    val qualifiers = required(shape, "qualifiers").arr.map(_.str).toSet
    val invalid = qualifiers -- Set("const", "volatile", "restrict")
    if (invalid.nonEmpty) unsupported("unsupported compiler qualifiers " + invalid.toSeq.sorted.mkString(","))
    val constant = qualifiers("const"); val volatile = qualifiers("volatile"); val restrict = qualifiers("restrict")
    val kind = text(shape, "kind")
    val raw: IType = kind match {
      case "Void" => new CBasicType(IBasicType.Kind.eVoid, 0)
      case "Bool" => new CBasicType(IBasicType.Kind.eBoolean, 0)
      case "Char_S" | "Char_U" => new CBasicType(IBasicType.Kind.eChar, 0)
      case "SChar" => new CBasicType(IBasicType.Kind.eChar, IBasicType.IS_SIGNED)
      case "UChar" => new CBasicType(IBasicType.Kind.eChar, IBasicType.IS_UNSIGNED)
      case "Short" => new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_SHORT)
      case "UShort" => new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_SHORT | IBasicType.IS_UNSIGNED)
      case "Int" => new CBasicType(IBasicType.Kind.eInt, 0)
      case "UInt" => new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_UNSIGNED)
      case "Long" => new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG)
      case "ULong" => new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG | IBasicType.IS_UNSIGNED)
      case "LongLong" => new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG_LONG)
      case "ULongLong" => new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG_LONG | IBasicType.IS_UNSIGNED)
      case "Int128" => new CBasicType(IBasicType.Kind.eInt128, 0)
      case "UInt128" => new CBasicType(IBasicType.Kind.eInt128, IBasicType.IS_UNSIGNED)
      case "Float" => new CBasicType(IBasicType.Kind.eFloat, 0)
      case "Double" => new CBasicType(IBasicType.Kind.eDouble, 0)
      case "LongDouble" => new CBasicType(IBasicType.Kind.eDouble, IBasicType.IS_LONG)
      case "Float128" => new CBasicType(IBasicType.Kind.eFloat128, 0)
      case "Pointer" =>
        val flags = (if (constant) CPointerType.IS_CONST else 0) |
          (if (volatile) CPointerType.IS_VOLATILE else 0) | (if (restrict) CPointerType.IS_RESTRICT else 0)
        return new CPointerType(value(required(shape, "pointee")), flags)
      case "FunctionProto" => new CFunctionType(value(required(shape, "result")),
        required(shape, "arguments").arr.map(value).toArray, required(shape, "variadic").bool)
      case "FunctionNoProto" => unsupported("old-style function type has no prototype argument contract")
      case "Record" =>
        val key = text(shape, "declaration_usr")
        recordTypes.getOrElseUpdate(key, new RecordBinding(declarationFor(shape, "RecordDecl"), shape))
      case "Enum" =>
        val key = shape.obj.get("declaration_identity").map(_.str).getOrElse(text(shape, "declaration_usr"))
        enumTypes.getOrElseUpdate(key, new EnumBinding(declarationFor(shape, "EnumDecl"), key))
      case "ConstantArray" =>
        val extent = required(shape, "extent").num
        if (extent < 0 || extent > 9007199254740991d || !extent.isWhole) unsupported("invalid or non-exact compiler array extent")
        new CArrayType(value(required(shape, "element")), constant, volatile, restrict, IntegralValue.create(extent.toLong))
      case "IncompleteArray" => new CArrayType(value(required(shape, "element")))
      case other => unsupported("unsupported compiler semantic type " + other)
    }
    if (qualifiers.nonEmpty && !Set("ConstantArray", "IncompleteArray")(kind))
      new CQualifierType(raw, constant, volatile, restrict)
    else raw
  }

  def binding(declarationId: String): IBinding = bindings.getOrElseUpdate(declarationId, {
    val declaration = syntax(declarationId)
    nodeKind(declaration) match {
      case "VarDecl" => new VariableBinding(declarationId)
      case "ParmVarDecl" => new ParameterBinding(declarationId)
      case "FunctionDecl" => new FunctionBinding(declarationId)
      case "FieldDecl" => new FieldBinding(declarationId)
      case "RecordDecl" | "EnumDecl" => value(required(facts(declarationId), "semantic_type")) match {
        case result: IBinding => result
        case _ => unsupported("declaration type is not a binding: " + declarationId)
      }
      case "TypedefDecl" => new TypedefBinding(declarationId)
      case "EnumConstantDecl" => new EnumeratorBinding(declarationId)
      case other => unsupported("unsupported declaration binding " + other)
    }
  })

  private abstract class CompilerBinding(val identifier: String) extends IBinding {
    protected val declaration: ujson.Value = syntax(identifier)
    protected val entry: ujson.Value = facts(identifier)
    protected def ownerBinding: IBinding = owners.getOrElse(identifier, unsupported("missing declaration ownership"))
      .map(binding).orNull
    protected def storage: String = declaration.obj.get("storageClass").map(_.str).getOrElse("")
    override def getName: String = text(entry, "name")
    override def getNameCharArray: Array[Char] = getName.toCharArray
    override def getLinkage: ILinkage = Linkage.C_LINKAGE
    override def getOwner: IBinding = ownerBinding
    override def getScope: IScope = unsupported("scope lookup is not implemented for " + getName)
    override def getAdapter[Target](adapter: Class[Target]): Target = unsupported("binding adapter " + adapter.getName)
    override def toString: String = getName
  }

  private class VariableBinding(identifier: String) extends CompilerBinding(identifier) with IVariable {
    override def getType: IType = value(required(entry, "semantic_type"))
    override def getInitialValue: IValue = {
      if (required(entry, "has_initializer").bool) unsupported("constant evaluation of variable initializer " + getName)
      null
    }
    override def isStatic: Boolean = storage == "static"
    override def isExtern: Boolean = storage == "extern"
    override def isAuto: Boolean = storage == "auto"
    override def isRegister: Boolean = storage == "register"
  }
  private final class ParameterBinding(identifier: String) extends VariableBinding(identifier) with IParameter
  private final class FieldBinding(identifier: String) extends VariableBinding(identifier) with IField {
    override def getCompositeTypeOwner: ICompositeType = ownerBinding match {
      case record: ICompositeType => record
      case _ => unsupported("field has no compiler record owner: " + getName)
    }
    override def getInitialValue: IValue = unsupported("C field initializer evaluation " + getName)
  }
  private final class FunctionBinding(identifier: String) extends CompilerBinding(identifier) with IFunction {
    override def getType: IFunctionType = value(required(entry, "semantic_type")) match {
      case function: IFunctionType => function
      case _ => unsupported("function declaration has non-function type")
    }
    override def getParameters: Array[IParameter] = children(declaration).filter(nodeKind(_) == "ParmVarDecl")
      .map(node => binding(text(node, "id")) match {
        case parameter: IParameter => parameter
        case _ => unsupported("function parameter has an invalid binding")
      }).toArray
    override def getFunctionScope: IScope = unsupported("function scope lookup " + getName)
    override def isStatic: Boolean = storage == "static"
    override def isExtern: Boolean = storage == "extern"
    override def isAuto: Boolean = storage == "auto"
    override def isRegister: Boolean = storage == "register"
    override def isInline: Boolean = declaration.obj.get("inline").exists(_.bool)
    override def takesVarArgs: Boolean = required(entry, "variadic").bool
    override def isNoReturn: Boolean = children(declaration).exists(node => nodeKind(node) == "NoReturnAttr")
    override def isNoDiscard: Boolean = children(declaration).exists(node => nodeKind(node) == "WarnUnusedResultAttr")
  }
  private final class TypedefBinding(identifier: String) extends CompilerBinding(identifier) with ITypedef {
    override def getType: IType = value(required(entry, "semantic_type"))
    override def isSameType(other: IType): Boolean = getType.isSameType(other)
    override def clone(): AnyRef = this
  }
  private final class RecordBinding(identifier: String, shape: ujson.Value)
      extends CompilerBinding(identifier) with ICompositeType {
    private val identity = text(shape, "declaration_usr")
    override def getKey: Int = text(shape, "tag") match {
      case "struct" => ICompositeType.k_struct
      case "union" => ICompositeType.k_union
      case other => unsupported("unsupported compiler record tag " + other)
    }
    override def isAnonymous: Boolean = !declaration.obj.get("name").exists(_.str.nonEmpty)
    override def getFields: Array[IField] = {
      if (!required(shape, "complete").bool) unsupported("fields requested for incomplete compiler record " + getName)
      children(declaration).filter(nodeKind(_) == "FieldDecl").map(node => binding(text(node, "id")) match {
        case field: IField => field
        case _ => unsupported("record child has invalid field binding")
      }).toArray
    }
    override def findField(name: String): IField = {
      val fields = getFields
      val matches = fields.filter(_.getName == name)
      if (matches.length > 1) unsupported("ambiguous compiler record field " + name)
      if (matches.isEmpty && fields.exists(_.getName.isEmpty)) unsupported("anonymous field lookup requires compiler resolution")
      matches.headOption.orNull
    }
    override def getCompositeScope: IScope = unsupported("composite scope lookup " + getName)
    override def isSameType(other: IType): Boolean = other match {
      case record: RecordBinding => identity == record.identity
      case alias: ITypedef => isSameType(alias.getType)
      case _ => false
    }
    override def clone(): AnyRef = this
  }
  private final class EnumBinding(identifier: String, private val identity: String)
      extends CompilerBinding(identifier) with IEnumeration {
    override def getEnumerators: Array[IEnumerator] = children(declaration).filter(nodeKind(_) == "EnumConstantDecl")
      .map(node => binding(text(node, "id")) match {
        case enumerator: IEnumerator => enumerator
        case _ => unsupported("enum child has invalid enumerator binding")
      }).toArray
    private def bounds: Seq[Long] = {
      val values = getEnumerators.toSeq.map(_.getValue.numberValue().longValue())
      if (values.isEmpty) unsupported("enumeration lacks value evidence")
      values
    }
    override def getMinValue: Long = bounds.min
    override def getMaxValue: Long = bounds.max
    override def isSameType(other: IType): Boolean = other match {
      case enumeration: EnumBinding => identity == enumeration.identity
      case alias: ITypedef => isSameType(alias.getType)
      case _ => false
    }
    override def clone(): AnyRef = this
  }
  private final class EnumeratorBinding(identifier: String) extends CompilerBinding(identifier) with IEnumerator {
    override def getType: IType = value(required(entry, "semantic_type"))
    override def getValue: IValue = {
      val number = BigInt(text(entry, "constant_value"))
      if (!number.isValidLong) unsupported("enumerator exceeds CDT integral value range")
      IntegralValue.create(number.longValue)
    }
  }
}
