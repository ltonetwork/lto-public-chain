package com.ltonetwork.state

import com.ltonetwork.utils.{Base58, Base64}

import scala.util.Try

case class ByteStr(arr: Array[Byte]) {
  override def equals(a: Any): Boolean = a match {
    case other: ByteStr => arr.sameElements(other.arr)
    case _              => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(arr)

  lazy val base58: String = Base58.encode(arr)
  lazy val base64: String = "base64:" + Base64.encode(arr)
  lazy val trim: String   = base58.toString.take(7) + "..."

  override lazy val toString: String = base58
}

object ByteStr {
  def decodeBase58(s: String): Try[ByteStr] = Base58.decode(s).map(ByteStr(_))
  def decodeBase64(s: String): Try[ByteStr] = Base64.decode(s).map(ByteStr(_))
  val empty: ByteStr                        = ByteStr(Array.emptyByteArray)

  implicit def toArr(byteStr: ByteStr): Array[Byte] = byteStr.arr
}
