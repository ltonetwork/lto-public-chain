package com.ltonetwork.account

import scala.util.{Failure, Success, Try}

case class KeyType private (id: Byte, length: Short, reference: String) {
  override def toString: String = reference
}

object KeyTypes {
  val ED25519: KeyType   = KeyType(1, 32, "ed25519")
  val SECP256K1: KeyType = KeyType(2, 33, "secp256k1")
  val SECP256R1: KeyType = KeyType(3, 33, "secp256r1")
  val BLS12_381: KeyType = KeyType(4, 48, "bls12-381")

  val all: Seq[KeyType] = Seq(
    ED25519,
    SECP256K1,
    SECP256R1,
    BLS12_381
  )

  val signing: Seq[KeyType] = Seq(
    ED25519,
    SECP256K1,
    SECP256R1
  )

  private val dictId        = all.map(f => f.id        -> f).toMap
  private val dictReference = all.map(f => f.reference -> f).toMap

  def keyType(id: Byte): Try[KeyType] =
    dictId
      .get(id)
      .map(Success(_))
      .getOrElse(Failure(new IndexOutOfBoundsException(s"Unknown key type id $id")))

  def keyType(reference: String): Try[KeyType] =
    dictReference
      .get(reference.toLowerCase())
      .map(Success(_))
      .getOrElse(Failure(new IndexOutOfBoundsException(s"Unknown key type '$reference'")))

  def keyType(reference: Option[String]): Try[KeyType] = reference.map(keyType).getOrElse(Success(ED25519))
}
