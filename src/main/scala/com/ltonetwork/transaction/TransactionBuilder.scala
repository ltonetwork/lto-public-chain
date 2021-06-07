package com.ltonetwork.transaction

import com.ltonetwork.account.{AddressScheme, PrivateKeyAccount}
import scala.util.{Failure, Success, Try}

trait TransactionBuilder {
  import TransactionParser._

  type TransactionT <: Transaction

  def typeId: Byte
  def supportedVersions: Set[Byte]
  def networkByte: Byte = AddressScheme.current.chainId

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount): TransactionT
  implicit def sign(tx: Either[ValidationError, TransactionT], signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    tx.map(unsigned => sign(unsigned, signer))

  def serializer(version: Byte): TransactionSerializer.For[TransactionT]

  def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] = MultipleVersions(typeId, supportedVersions).parseHeader(bytes)
  def parseBytes(bytes: Array[Byte]): Try[TransactionT] = (for {
    (version, end) <- parseHeader(bytes)
    tx             <- serializer(version).parseBytes(version, bytes.drop(end))
  } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))

  protected object UnknownSerializer extends TransactionSerializer.Unknown[TransactionT]
}

object TransactionBuilder {
  abstract class For[T <: Transaction] extends TransactionBuilder {
    override type TransactionT = T
  }
}
