package com.ltonetwork.account

case class KeyType private (id: Byte, length: Short, reference: String)

object KeyTypes {
  val ED25519: KeyType = KeyType(1, 32, "ed25519")

  private val dict = Seq(
    ED25519
  ).map(f => f.id -> f).toMap

  def keyType(id: Byte): Option[KeyType] = dict.get(id)
}
