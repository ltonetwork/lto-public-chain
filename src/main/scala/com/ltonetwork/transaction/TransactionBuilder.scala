package com.ltonetwork.transaction

import scala.util.Try

trait TransactionBuilder {
  type TransactionT <: Transaction

  def typeId: Byte
  def supportedVersions: Set[Byte]

  def serializer(version: Byte): TransactionSerializer.For[TransactionT]
  def parseBytes(bytes: Array[Byte]): Try[TransactionT]

  protected object UnknownSerializer extends TransactionSerializer.Unknown[TransactionT]
}

object TransactionBuilder {
  abstract class For[T <: Transaction] extends TransactionBuilder {
    override type TransactionT = T
  }
}
