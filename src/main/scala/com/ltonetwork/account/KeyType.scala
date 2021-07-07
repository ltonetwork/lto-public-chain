package com.ltonetwork.account

import scala.util.{Failure, Success, Try}

case class KeyType private (id: Byte, length: Short, reference: String)  {
  override def toString: String = reference
}

object KeyTypes {
  val ED25519: KeyType = KeyType(1, 32, "ed25519")

  private val dict = Seq(
    ED25519
  ).map(f => f.id -> f).toMap

  def keyType(id: Byte): Try[KeyType] = dict.get(id)
    .map(Success(_)).getOrElse(Failure(new IndexOutOfBoundsException(s"Unknown key type id $id")))
}
