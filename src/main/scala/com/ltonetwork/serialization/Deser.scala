package com.ltonetwork.serialization

import com.google.common.primitives.{Bytes, Shorts}
import com.ltonetwork.account.PublicKeyAccount

// Todo remove parse methods and only use ByteBuffer with methods from package
object Deser {

  def serializeBoolean(b: Boolean): Array[Byte] = if (b) Array(1: Byte) else Array(0: Byte)

  def serializeArray(b: Array[Byte]): Array[Byte] = Shorts.toByteArray(b.length.toShort) ++ b

  def parseArraySize(bytes: Array[Byte], position: Int): (Array[Byte], Int) = {
    val length = Shorts.fromByteArray(bytes.slice(position, position + 2))
    (bytes.slice(position + 2, position + 2 + length), position + 2 + length)
  }

  def parseByteArrayOption(bytes: Array[Byte], position: Int, length: Int): (Option[Array[Byte]], Int) = {
    if (bytes.slice(position, position + 1).head == (1: Byte)) {
      val b = bytes.slice(position + 1, position + 1 + length)
      (Some(b), position + 1 + length)
    } else (None, position + 1)
  }

  def parseOption[T](bytes: Array[Byte], position: Int)(deser: Array[Byte] => T): (Option[T], Int) = {
    if (bytes.slice(position, position + 1).head == (1: Byte)) {
      val (arr, arrPosEnd) = parseArraySize(bytes, position + 1)
      (Some(deser(arr)), arrPosEnd)
    } else (None, position + 1)
  }

  def parseArrays(bytes: Array[Byte]): Seq[Array[Byte]] = {
    val length = Shorts.fromByteArray(bytes.slice(0, 2))
    val r = (0 until length).foldLeft((Seq.empty[Array[Byte]], 2)) {
      case ((acc, pos), _) =>
        val (arr, nextPos) = parseArraySize(bytes, pos)
        (acc :+ arr, nextPos)
    }
    r._1
  }

  def parseArraysPos(bytes: Array[Byte]): (Seq[Array[Byte]], Int) = {
    val length = Shorts.fromByteArray(bytes.slice(0, 2))
    val r = (0 until length).foldLeft((Seq.empty[Array[Byte]], 2)) {
      case ((acc, pos), _) =>
        val (arr, nextPos) = parseArraySize(bytes, pos)
        (acc :+ arr, nextPos)
    }
    r
  }

  def serializeOption[T](b: Option[T])(ser: T => Array[Byte]): Array[Byte] = b.map(a => (1: Byte) +: ser(a)).getOrElse(Array(0: Byte))

  def serializeArrays(bs: Seq[Array[Byte]]): Array[Byte] = Shorts.toByteArray(bs.length.toShort) ++ Bytes.concat(bs.map(serializeArray): _*)

  def serializeArraysWithoutLength(bs: Seq[Array[Byte]]): Array[Byte] = Bytes.concat(bs: _*)

  def serializeList[T](list: List[T])(ser: T => Array[Byte]): Array[Byte] =
    Shorts.toByteArray(list.length.toShort) ++ list.map(ser(_)).reduceOption((a, b) => a ++ b).getOrElse(Array.emptyByteArray)

  def serializeAccount(account: PublicKeyAccount): Array[Byte] = Bytes.concat(Array(account.keyType.id), account.publicKey)

  def serializeMap[T1, T2](m: Map[T1, T2])(ser: (T1, T2) => Array[Byte]): Array[Byte] =
    Shorts.toByteArray(m.toSeq.length.toShort) ++ Bytes.concat(m.toSeq.map { case (k, v) => ser(k, v) }: _*)
}
