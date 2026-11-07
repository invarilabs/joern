package io.joern.clang2cpg.parser.clang

import io.joern.clang2cpg.parser.memory.NativeValue

/** Normalize only Clang's recursively elided raw location fields when comparing one declaration identity. */
object NativeDeclarationShape {
  private def locationField(name: String): Boolean = name == "loc" || name == "range"

  private def semanticShape(left: NativeValue, right: NativeValue): Boolean = left.objOpt match {
    case Some(leftFields) => right.objOpt.exists { rightFields =>
      leftFields.keysIterator.count(name => !locationField(name)) ==
        rightFields.keysIterator.count(name => !locationField(name)) &&
      leftFields.iterator.filterNot(entry => locationField(entry._1)).forall { case (name, value) =>
        rightFields.get(name).exists(other => semanticShape(value, other))
      }
    }
    case None => left.arrOpt match {
      case Some(leftItems) => right.arrOpt.exists { rightItems =>
        leftItems.size == rightItems.size && leftItems.iterator.zip(rightItems.iterator).forall {
          case (leftChild, rightChild) => semanticShape(leftChild, rightChild)
        }
      }
      case None => left == right
    }
  }

  def requireEquivalent(previous: NativeValue, repeated: NativeValue): Unit = {
    val previousIdentity = previous.obj.get("id").flatMap(_.strOpt).filter(_.nonEmpty)
    val repeatedIdentity = repeated.obj.get("id").flatMap(_.strOpt).filter(_.nonEmpty)
    require(previousIdentity.nonEmpty && repeatedIdentity.nonEmpty, "Clang declaration lacks compiler identity")
    require(previousIdentity == repeatedIdentity && semanticShape(previous, repeated),
      s"Conflicting Clang declaration ${previousIdentity.get}: semantic AST shapes differ")
  }
}
