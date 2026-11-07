package io.joern.clang2cpg.parser.memory

import java.util.{LinkedHashMap as JavaMap, ArrayList as JavaList}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Typed in-memory JNI values. JSON is encoded only when writing existing CPG property strings. */
final class NativeValue private (private val value: AnyRef) {
  private def wrong(expected: String): Nothing =
    throw new IllegalArgumentException("Native Clang value requires " + expected + "; received " + value.getClass.getName)

  lazy val obj: mutable.LinkedHashMap[String, NativeValue] = value match {
    case entries: java.util.Map[?, ?] =>
      val result = mutable.LinkedHashMap.empty[String, NativeValue]
      entries.entrySet().asScala.foreach { entry =>
        val key = entry.getKey match {
          case name: String => name
          case _ => throw new IllegalArgumentException("Native Clang object keys must be strings")
        }
        result(key) = NativeValue.fromJava(entry.getValue.asInstanceOf[AnyRef])
      }
      result
    case _ => wrong("an object")
  }
  lazy val arr: mutable.ArrayBuffer[NativeValue] = value match {
    case entries: java.util.List[?] =>
      mutable.ArrayBuffer.from(entries.asScala.map(entry => NativeValue.fromJava(entry.asInstanceOf[AnyRef])))
    case _ => wrong("an array")
  }
  def apply(key: String): NativeValue = obj.getOrElse(key,
    throw new IllegalArgumentException("Native Clang object lacks required field: " + key))
  def apply(index: Int): NativeValue = {
    require(index >= 0 && index < arr.size, "Native Clang array index out of range: " + index)
    arr(index)
  }
  def str: String = strOpt.getOrElse(wrong("a string"))
  def num: Double = numOpt.getOrElse(wrong("a number"))
  def bool: Boolean = boolOpt.getOrElse(wrong("a Boolean"))
  def strOpt: Option[String] = value match { case text: String => Some(text); case _ => None }
  def numOpt: Option[Double] = value match { case number: java.lang.Number => Some(number.doubleValue()); case _ => None }
  def boolOpt: Option[Boolean] = value match { case flag: java.lang.Boolean => Some(flag.booleanValue()); case _ => None }
  def objOpt: Option[mutable.LinkedHashMap[String, NativeValue]] = value match {
    case _: java.util.Map[?, ?] => Some(obj)
    case _ => None
  }
  def arrOpt: Option[mutable.ArrayBuffer[NativeValue]] = value match {
    case _: java.util.List[?] => Some(arr)
    case _ => None
  }
  private def propertyValue: ujson.Value = value match {
    case _: java.util.Map[?, ?] => ujson.Obj.from(obj.iterator.map { case (key, entry) => key -> entry.propertyValue })
    case _: java.util.List[?] => ujson.Arr.from(arr.iterator.map(_.propertyValue))
    case text: String => ujson.Str(text)
    case number: java.lang.Number => ujson.Num(number.doubleValue())
    case flag: java.lang.Boolean => ujson.Bool(flag.booleanValue())
    case _ => wrong("a supported native value")
  }
  /** Preserves the established string encoding of graph metadata; never used as an AST transport. */
  def compact: String = ujson.write(propertyValue)
  override def equals(other: Any): Boolean = other match {
    case entry: NativeValue => value match {
      case _: java.util.Map[?, ?] => entry.objOpt.exists(_ == obj)
      case _: java.util.List[?] => entry.arrOpt.exists(_ == arr)
      case _: java.lang.Number => entry.numOpt == numOpt
      case _ => value == entry.value
    }
    case _ => false
  }
  override def hashCode(): Int = value match {
    case _: java.util.Map[?, ?] => obj.hashCode()
    case _: java.util.List[?] => arr.hashCode()
    case _: java.lang.Number => num.hashCode()
    case _ => value.hashCode()
  }
  override def toString: String = value match {
    case _: java.util.Map[?, ?] => "NativeObject(" + obj.size + " fields)"
    case _: java.util.List[?] => "NativeArray(" + arr.size + " values)"
    case _ => value.toString
  }
}

object NativeValue {
  def write(value: NativeValue): String = value.compact
  def fromJava(value: AnyRef): NativeValue = {
    require(value != null, "Native Clang returned a null value")
    value match {
      case entry: NativeValue => entry
      case number: java.lang.Double =>
        require(java.lang.Double.isFinite(number.doubleValue()), "Native Clang numeric metadata must be finite")
        new NativeValue(value)
      case number: java.lang.Float =>
        require(java.lang.Float.isFinite(number.floatValue()), "Native Clang numeric metadata must be finite")
        new NativeValue(value)
      case _: java.util.Map[?, ?] | _: java.util.List[?] | _: String | _: java.lang.Boolean |
          _: java.lang.Long | _: java.lang.Integer => new NativeValue(value)
      case other => throw new IllegalArgumentException("Unsupported native Clang value: " + other.getClass.getName)
    }
  }
  private[memory] def constructed(value: Any): NativeValue = value match {
    case entry: NativeValue => entry
    case text: String => NStr(text)
    case flag: Boolean => NBool(flag)
    case number: Byte => NNum(number.toDouble)
    case number: Short => NNum(number.toDouble)
    case number: Int => NNum(number.toDouble)
    case number: Long => NNum(number.toDouble)
    case number: Float => NNum(number.toDouble)
    case number: Double => NNum(number)
    case null => throw new IllegalArgumentException("Native Clang object cannot contain null")
    case other => throw new IllegalArgumentException("Unsupported constructed Clang value: " + other.getClass.getName)
  }
}
object NStr { def apply(value: String): NativeValue = NativeValue.fromJava(value) }
object NNum {
  def apply(value: Double): NativeValue = {
    require(java.lang.Double.isFinite(value), "Native Clang numeric metadata must be finite")
    NativeValue.fromJava(java.lang.Double.valueOf(value))
  }
}
object NBool { def apply(value: Boolean): NativeValue = NativeValue.fromJava(java.lang.Boolean.valueOf(value)) }
object NObj {
  def apply(fields: (String, Any)*): NativeValue = {
    val result = new JavaMap[String, AnyRef]()
    fields.foreach { case (key, value) =>
      require(key != null && !result.containsKey(key), "Native Clang object has a null or duplicate key: " + key)
      result.put(key, NativeValue.constructed(value))
    }
    NativeValue.fromJava(result)
  }
  def from(fields: IterableOnce[(String, Any)]): NativeValue = apply(fields.iterator.toSeq*)
}
object NArr {
  def apply(values: Any*): NativeValue = from(values)
  def from(values: IterableOnce[Any]): NativeValue = {
    val result = new JavaList[AnyRef]()
    values.iterator.foreach(value => result.add(NativeValue.constructed(value)))
    NativeValue.fromJava(result)
  }
}
