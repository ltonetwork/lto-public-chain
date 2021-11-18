package com.ltonetwork.transaction

import com.ltonetwork.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}

import scala.util.Try

trait TransactionBuilder {
  import TransactionParser._

  type TransactionT <: Transaction

  def typeId: Byte
  def supportedVersions: Set[Byte]
  def networkByte: Byte = AddressScheme.current.chainId

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT
  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount): TransactionT = sign(tx, signer, None)
  implicit def sign(tx: Either[ValidationError, TransactionT],
                    signer: PrivateKeyAccount,
                    sponsor: Option[PublicKeyAccount]): Either[ValidationError, TransactionT] =
    tx.map(unsigned => sign(unsigned, signer, sponsor))
  implicit def sign(tx: Either[ValidationError, TransactionT], signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    tx.map(unsigned => sign(unsigned, signer, None))

  def serializer(version: Byte): TransactionSerializer.For[TransactionT]

  def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] = MultipleVersions(typeId, supportedVersions).parseHeader(bytes)
  def parseBytes(bytes: Array[Byte]): Try[TransactionT] =
    for {
      (version, end) <- parseHeader(bytes)
      tx             <- serializer(version).parseBytes(version, bytes.drop(end))
    } yield tx

  protected object UnknownSerializer extends TransactionSerializer.Unknown[TransactionT](typeId)
}

object TransactionBuilder {
  abstract class For[T <: Transaction] extends TransactionBuilder {
    override type TransactionT = T
  }
}
