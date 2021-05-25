package com.ltonetwork.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.{Address, AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.GenericError
import monix.eval.Coeval
import play.api.libs.json._
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}
abstract class SponsorshipTransactionBase(val version: Byte,
                                          val chainId: Byte,
                                          val sender: PublicKeyAccount,
                                          val recipient: Address,
                                          val fee: Long,
                                          val timestamp: Long,
                                          val proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(builder.typeId, version, chainId),
      sender.publicKey,
      recipient.bytes.arr,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version"   -> version,
      "recipient" -> recipient.stringRepr,
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

}

case class SponsorshipTransaction private (override val version: Byte,
                                           override val chainId: Byte,
                                           override val sender: PublicKeyAccount,
                                           override val recipient: Address,
                                           override val fee: Long,
                                           override val timestamp: Long,
                                           override val proofs: Proofs)
    extends SponsorshipTransactionBase(version, chainId, sender, recipient, fee, timestamp, proofs) {
  override def builder: TransactionParser = SponsorshipTransaction
}
case class SponsorshipCancelTransaction private (override val version: Byte,
                                                 override val chainId: Byte,
                                                 override val sender: PublicKeyAccount,
                                                 override val recipient: Address,
                                                 override val fee: Long,
                                                 override val timestamp: Long,
                                                 override val proofs: Proofs)
    extends SponsorshipTransactionBase(version, chainId, sender, recipient, fee, timestamp, proofs) {
  override def builder: TransactionParser = SponsorshipCancelTransaction
}
object SponsorshipTransactionBase {

  type SignedCtor[T] = (Byte, PublicKeyAccount, Address, Long, Long, PrivateKeyAccount) => Either[ValidationError, T]
  type CreateCtor[T] = (Byte, PublicKeyAccount, Address, Long, Long, Proofs) => Either[ValidationError, T]

  val supportedVersions: Set[Byte] = Set(1)
  val HashLength                   = 64

  def networkByte = AddressScheme.current.chainId

  def parseTail[T](version: Byte, bytes: Array[Byte], create: (Byte, Byte, PublicKeyAccount, Address, Long, Long, Proofs) => T): Try[T] =
    Try {

      val chainId = bytes(0)
      val p0      = KeyLength
      val sender  = PublicKeyAccount(bytes.slice(1, p0 + 1))
      val txEi = for {
        recipient <- Address.fromBytes(bytes.slice(p0 + 1, p0 + 1 + Address.AddressLength))
        recipientEnd = p0 + 1 + Address.AddressLength
        s1           = recipientEnd
        timestamp    = Longs.fromByteArray(bytes.drop(s1))
        fee    = Longs.fromByteArray(bytes.drop(s1 + 8))
        proofs <- Proofs.fromBytes(bytes.drop(s1 + 16))
        _      <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
        _      <- validate(version, sender, recipient, fee)
        tx = create(version, chainId, sender, recipient, fee, timestamp, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def validate(version: Byte, sender: PublicKeyAccount, party: Address, fee: Long): Either[ValidationError, Unit] = {
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else if (sender.address == party.address) {
      Left(GenericError("Can't sponsor oneself"))
    } else {
      Right(())
    }
  }
}

object SponsorshipTransaction extends TransactionParserFor[SponsorshipTransaction] with TransactionParser.MultipleVersions {
  override def typeId: Byte                 = 18
  override def supportedVersions: Set[Byte] = Set(1: Byte)
  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[SponsorshipTransaction] =
    SponsorshipTransactionBase.parseTail(version, bytes, SponsorshipTransaction.apply)

  def signed(version: Byte,
             sender: PublicKeyAccount,
             recipient: Address,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    SponsorshipTransactionBase.validate(version, sender, recipient, fee).map { _ =>
      val uns = SponsorshipTransaction(version, SponsorshipTransactionBase.networkByte, sender, recipient, fee, timestamp, Proofs.empty)
      uns.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, uns.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 recipient: Address,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(version, sender, recipient, fee, timestamp, sender)
  }

  def create(version: Byte,
             sender: PublicKeyAccount,
             recipient: Address,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, SponsorshipTransaction] =
    SponsorshipTransactionBase
      .validate(version, sender, recipient, fee)
      .map(_ => SponsorshipTransaction(version, SponsorshipTransactionBase.networkByte, sender, recipient, fee, timestamp, proofs))
}

object SponsorshipCancelTransaction extends TransactionParserFor[SponsorshipCancelTransaction] with TransactionParser.MultipleVersions {
  override def typeId: Byte                 = 19
  override def supportedVersions: Set[Byte] = Set(1: Byte)
  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[SponsorshipCancelTransaction] =
    SponsorshipTransactionBase.parseTail(version, bytes, SponsorshipCancelTransaction.apply)

  def signed(version: Byte,
             sender: PublicKeyAccount,
             recipient: Address,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    SponsorshipTransactionBase.validate(version, sender, recipient, fee).map { _ =>
      val uns = SponsorshipCancelTransaction(version, SponsorshipTransactionBase.networkByte, sender, recipient, fee, timestamp, Proofs.empty)
      uns.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, uns.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 recipient: Address,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(version, sender, recipient, fee, timestamp, sender)
  }
  def create(version: Byte,
             sender: PublicKeyAccount,
             recipient: Address,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, SponsorshipCancelTransaction] =
    SponsorshipTransactionBase
      .validate(version, sender, recipient, fee)
      .map(_ => SponsorshipCancelTransaction(version, SponsorshipTransactionBase.networkByte, sender, recipient, fee, timestamp, proofs))
}
