package com.ltonetwork.transaction

import com.ltonetwork.state._
import monix.eval.Coeval
import com.ltonetwork.serialization.{BytesSerializable, JsonSerializable}

trait Transaction extends BytesSerializable with JsonSerializable {
  val id: Coeval[ByteStr]

  def builder: TransactionParser
  def timestamp: Long
  def fee: Long

  override def toString: String = json().toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()
}

object Transaction {
  type Type = Byte
}
