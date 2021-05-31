package com.ltonetwork.transaction

import com.ltonetwork.account.AddressScheme
import scala.util.{Try, Failure, Success}

trait TransactionBuilder {
  import TransactionParser._

  type TransactionT <: Transaction

  def typeId: Byte
  def supportedVersions: Set[Byte]
  def networkByte: Byte = AddressScheme.current.chainId

  def serializer(version: Byte): TransactionSerializer.For[TransactionT]

  def parseBytes(bytes: Array[Byte]): Try[TransactionT] = (for {
    (version, end) <- MultipleVersions(typeId, supportedVersions).parseHeader(bytes)
    tx             <- serializer(version).parseBytes(version, bytes.drop(end))
  } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))

  protected object UnknownSerializer extends TransactionSerializer.Unknown[TransactionT]
}

object TransactionBuilder {
  abstract class For[T <: Transaction] extends TransactionBuilder {
    override type TransactionT = T
  }
}
