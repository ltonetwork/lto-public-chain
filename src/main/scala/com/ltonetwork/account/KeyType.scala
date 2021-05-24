package com.ltonetwork.account

case class KeyType private (id: Byte, length: Short, description: String)

object KeyTypes {
  val ED25519   = KeyType(1, 32, "ED25519")
  val SECP256R1 = KeyType(2, 33, "secp256r1")
  val SECP256K1 = KeyType(3, 33, "secp256k1")

  private val dict = Seq(
    ED25519,
    SECP256R1,
    SECP256K1
  ).map(f => f.id -> f).toMap

  def keyType(id: Byte): Option[KeyType] = dict.get(id)
}
