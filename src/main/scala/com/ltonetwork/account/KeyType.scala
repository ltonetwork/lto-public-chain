package com.ltonetwork.account

import scala.util.{Failure, Success, Try}
import scala.util.control.Exception.allCatch

case class KeyType private (id: Byte, length: Short, reference: String) {
  override def toString: String = reference
}

object KeyTypes {
  val ED25519: KeyType   = KeyType(1, 32, "ed25519")
  val SECP256K1: KeyType = KeyType(2, 33, "SECP256k1")
  val SECP256R1: KeyType = KeyType(3, 33, "SECP256r1")

  private val all = Seq(
    ED25519,
    SECP256K1,
    SECP256R1
  )

  private val dictId        = all.map(f => f.id        -> f).toMap
  private val dictReference = all.map(f => f.reference -> f).toMap

  private def isByte(str: String): Boolean = (allCatch opt str.toByte).isDefined

  def keyType(id: Byte): Try[KeyType] =
    dictId
      .get(id)
      .map(Success(_))
      .getOrElse(Failure(new IndexOutOfBoundsException(s"Unknown key type id $id")))

  def keyType(reference: String): Try[KeyType] =
    (if (isByte(reference)) dictId.get(reference.toByte) else dictReference.get(reference))
      .map(Success(_))
      .getOrElse(Failure(new IndexOutOfBoundsException(s"Unknown key type '$reference'")))

  def keyType(reference: Option[String]): Try[KeyType] = reference.map(keyType).getOrElse(Success(ED25519))
}
