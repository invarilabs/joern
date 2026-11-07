package io.joern.clang2cpg.parser.clang

/** Compare repeated compiler declaration renderings; native evidence owns their source locations. */
object ClangDeclarationShape {
  private def locationField(name: String): Boolean = name == "loc" || name == "range"

  private def semanticShape(left: ujson.Value, right: ujson.Value): Boolean = (left, right) match {
    case (leftObject: ujson.Obj, rightObject: ujson.Obj) =>
      val leftFields = leftObject.obj
      val rightFields = rightObject.obj
      leftFields.keysIterator.count(name => !locationField(name)) ==
        rightFields.keysIterator.count(name => !locationField(name)) &&
        leftFields.iterator.filterNot(entry => locationField(entry._1)).forall { case (name, value) =>
          rightFields.get(name).exists(other => semanticShape(value, other))
        }
    case (leftArray: ujson.Arr, rightArray: ujson.Arr) =>
      leftArray.arr.size == rightArray.arr.size &&
        leftArray.arr.iterator.zip(rightArray.arr.iterator).forall { case (leftChild, rightChild) =>
          semanticShape(leftChild, rightChild)
        }
    case _ => left == right
  }

  def requireEquivalent(previous: ujson.Value, repeated: ujson.Value): Unit = {
    val previousIdentity = previous.obj.get("id").flatMap(_.strOpt).filter(_.nonEmpty)
    val repeatedIdentity = repeated.obj.get("id").flatMap(_.strOpt).filter(_.nonEmpty)
    require(previousIdentity.nonEmpty && repeatedIdentity.nonEmpty, "Clang declaration lacks compiler identity")
    require(
      previousIdentity == repeatedIdentity && semanticShape(previous, repeated),
      s"Conflicting Clang declaration ${previousIdentity.get}: semantic AST shapes differ"
    )
  }
}
