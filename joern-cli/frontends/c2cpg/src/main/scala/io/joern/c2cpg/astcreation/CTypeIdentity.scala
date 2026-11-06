package io.joern.c2cpg.astcreation

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.ICInternalBinding

import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.IdentityHashMap

final class CTypeEvidenceException(message: String, cause: Throwable = null)
    extends IllegalArgumentException(s"C type evidence: $message", cause)

/** Projects C record and enum bindings before their tag/typedef spelling is lost in string-keyed graph links. */
final class CTypeIdentity {
  private val recordNames      = new IdentityHashMap[ICompositeType, String]()
  private val enumerationNames = new IdentityHashMap[IEnumeration, String]()
  private val origins          = scala.collection.mutable.LinkedHashMap.empty[String, IType]

  private def fail(message: String): Nothing = throw new CTypeEvidenceException(message)

  private def origin(node: IASTNode): String = {
    val location = node.getFileLocation
    if (location == null) fail(s"declaration has no physical source location: ${node.getRawSignature}")
    val path       = Paths.get(location.getFileName).toAbsolutePath.normalize.toString.replace('\\', '/')
    val expansions = node.getNodeLocations.toList.collect { case macroLocation: IASTMacroExpansionLocation =>
      s"${macroLocation.getNodeOffset}:${macroLocation.getNodeLength}"
    }
    s"$path:${location.getNodeOffset}:${expansions.mkString(",")}"
  }

  private def key(kind: String, node: IASTNode): String = {
    val digest = MessageDigest
      .getInstance("SHA-256")
      .digest(s"$kind:${origin(node)}".getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
    s"<type>c_${kind}_$digest"
  }

  private def owningSpecifier(name: IASTName): IASTNode = name.getParent match {
    case specifier: IASTCompositeTypeSpecifier  => specifier
    case specifier: IASTEnumerationSpecifier    => specifier
    case specifier: IASTElaboratedTypeSpecifier => specifier
    case other => fail(s"record declaration has an unexpected owner: ${other.getClass.getName}")
  }

  private def declarationOwner(node: IASTNode): IASTNode = node match {
    case name: IASTName                         => owningSpecifier(name)
    case specifier: IASTCompositeTypeSpecifier  => specifier
    case specifier: IASTEnumerationSpecifier    => specifier
    case specifier: IASTElaboratedTypeSpecifier => specifier
    case other => fail(s"record binding has an unexpected declaration: ${other.getClass.getName}")
  }

  private def canonicalDeclaration(record: ICompositeType): IASTNode = record match {
    case binding: ICInternalBinding =>
      // CDT resolves a forward tag's complete definition lazily during member lookup.
      val _          = record.getFields
      val definition = binding.getDefinition
      if (definition != null) declarationOwner(definition)
      else {
        // Opaque tags have declarations but no definition; all declarations share this binding.
        val declarations = binding.getDeclarations
        if (declarations == null || declarations.isEmpty) fail(s"record binding has no declaration: ${record.getName}")
        declarations.toList.map(declarationOwner).sortBy(origin).head
      }
    case other => fail(s"record has no source binding identity: ${other.getClass.getName}")
  }

  def recordName(record: ICompositeType): String = {
    if (record.isInstanceOf[IProblemBinding]) fail(s"unresolved record binding: ${record.getName}")
    val cached = recordNames.get(record)
    if (cached != null) cached
    else {
      val name = key(s"record${record.getKey}", canonicalDeclaration(record))
      origins.get(name).foreach { previous =>
        if (!previous.isSameType(record)) fail(s"distinct record bindings share declaration identity: $name")
      }
      origins(name) = record
      recordNames.put(record, name)
      name
    }
  }

  private def enumerationDeclaration(enumeration: IEnumeration): IASTNode = enumeration match {
    case binding: ICInternalBinding =>
      val _          = enumeration.getEnumerators
      val definition = binding.getDefinition
      if (definition != null) declarationOwner(definition)
      else {
        val declarations = binding.getDeclarations
        if (declarations == null || declarations.isEmpty)
          fail(s"enum binding has no declaration: ${enumeration.getName}")
        declarations.toList.map(declarationOwner).sortBy(origin).head
      }
    case other => fail(s"enum has no source binding identity: ${other.getClass.getName}")
  }

  def enumerationName(enumeration: IEnumeration): String = {
    val cached = enumerationNames.get(enumeration)
    if (cached != null) cached
    else {
      val name = key("enum", enumerationDeclaration(enumeration))
      origins.get(name).foreach { previous =>
        if (!previous.isSameType(enumeration)) fail(s"distinct enum bindings share declaration identity: $name")
      }
      origins(name) = enumeration
      enumerationNames.put(enumeration, name)
      name
    }
  }

  /** Forward declarations retain their own origin and alias the resolved nominal identity. */
  def declarationName(specifier: IASTDeclSpecifier): Option[String] = specifier match {
    case composite: IASTCompositeTypeSpecifier =>
      composite.getName.resolveBinding() match {
        case record: ICompositeType => Some(recordName(record))
        case other => fail(s"composite declaration has no record binding: ${composite.getRawSignature}: $other")
      }
    case enumeration: IASTEnumerationSpecifier =>
      enumeration.getName.resolveBinding() match {
        case binding: IEnumeration => Some(enumerationName(binding))
        case other => fail(s"enum declaration has no enum binding: ${enumeration.getRawSignature}: $other")
      }
    case elaborated: IASTElaboratedTypeSpecifier =>
      elaborated.getName.resolveBinding() match {
        case record: ICompositeType =>
          val declaration = canonicalDeclaration(record)
          Some(if (origin(elaborated) == origin(declaration)) recordName(record) else key("declaration", elaborated))
        case enumeration: IEnumeration =>
          val declaration = enumerationDeclaration(enumeration)
          Some(
            if (origin(elaborated) == origin(declaration)) enumerationName(enumeration)
            else key("declaration", elaborated)
          )
        case other => fail(s"elaborated declaration has no nominal binding: ${elaborated.getRawSignature}: $other")
      }
    case _ => None
  }

  private def arraySuffix(array: IArrayType): String = {
    val size = array.getSize
    if (size == null) "[]" // The compiler represents a genuinely incomplete array without an extent.
    else {
      val number = size.numberValue()
      val extent = if (number != null) number.toString else new String(size.getSignature)
      if (extent.isEmpty) fail("array has an extent value but no compiler extent representation")
      s"[$extent]"
    }
  }

  /** None means a valid non-nominal type, never a failed declaration/binding lookup. */
  def typeName(nativeType: IType): Option[String] = {
    val visited                                 = new IdentityHashMap[IType, java.lang.Boolean]()
    def project(current: IType): Option[String] = {
      if (current == null) fail("compiler returned a null type")
      if (visited.put(current, java.lang.Boolean.TRUE) != null) fail("cyclic typedef or type wrapper")
      current match {
        case _: IProblemType           => fail("compiler returned an unresolved type")
        case alias: ITypedef           => project(alias.getType)
        case qualifier: IQualifierType => project(qualifier.getType)
        case pointer: IPointerType     => project(pointer.getType).map(_ + "*")
        case array: IArrayType         => project(array.getType).map(_ + arraySuffix(array))
        case record: ICompositeType    => Some(recordName(record))
        case enumeration: IEnumeration => Some(enumerationName(enumeration))
        case _                         => None
      }
    }
    project(nativeType)
  }
}
