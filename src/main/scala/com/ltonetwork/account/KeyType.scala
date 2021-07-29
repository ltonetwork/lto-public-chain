package com.ltonetwork.account

import scala.util.{Failure, Success, Try}

case class KeyType private (id: Byte, length: Short, reference: String)  {
  override def toString: String = reference
}

object KeyTypes {
  val ED25519: KeyType = KeyType(1, 32, "ed25519")

  private val all = Seq(
    ED25519
  )

  private val dictId = all.map(f => f.id -> f).toMap
  private val dictReference = all.map(f => f.reference -> f).toMap

  def keyType(id: Byte): Try[KeyType] = dictId.get(id)
    .map(Success(_)).getOrElse(Failure(new IndexOutOfBoundsException(s"Unknown key type id $id")))

  def keyType(reference: String): Try[KeyType] = dictReference.get(reference)
    .map(Success(_)).getOrElse(Failure(new IndexOutOfBoundsException(s"Unknown key type '$reference'")))

  def keyType(reference: Option[String]): Try[KeyType] = reference.map(keyType).getOrElse(Success(ED25519))
}
