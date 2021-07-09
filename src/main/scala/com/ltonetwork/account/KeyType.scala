package com.ltonetwork.account

import scala.util.{Failure, Success, Try}

case class KeyType private (id: Byte, length: Short, reference: String)  {
  override def toString: String = reference
}

object KeyTypes {
  val ED25519: KeyType = KeyType(1, 32, "ed25519")
  val SECP256K1: KeyType = KeyType(1, 33, "SECP256k1")
  val SECP256R1: KeyType = KeyType(1, 33, "SECP256r1")

  private val dict = Seq(
    ED25519,
    SECP256K1,
    SECP256R1
  ).map(f => f.id -> f).toMap

  def keyType(id: Byte): Try[KeyType] = dict.get(id)
    .map(Success(_)).getOrElse(Failure(new IndexOutOfBoundsException(s"Unknown key type id $id")))
}
