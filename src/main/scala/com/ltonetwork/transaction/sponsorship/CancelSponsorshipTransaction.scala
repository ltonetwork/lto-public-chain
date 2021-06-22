package com.ltonetwork.transaction.sponsorship

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.transaction.{Proofs, TransactionBuilder, TransactionSerializer, ValidationError}
import monix.eval.Coeval
import play.api.libs.json.JsObject
import scorex.crypto.signatures.Curve25519.SignatureLength

import scala.util.{Failure, Success, Try}

case class CancelSponsorshipTransaction private (version: Byte,
                                                 chainId: Byte,
                                                 timestamp: Long,
                                                 sender: PublicKeyAccount,
                                                 fee: Long,
                                                 recipient: Address,
                                                 sponsor: Option[PublicKeyAccount],
                                                 proofs: Proofs)
    extends SponsorshipTransactionBase {

  override def builder: TransactionBuilder.For[CancelSponsorshipTransaction] = CancelSponsorshipTransaction
  private def serializer: TransactionSerializer.For[CancelSponsorshipTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  override val json: Coeval[JsObject] = Coeval.evalOnce(serializer.toJson(this))
}

object CancelSponsorshipTransaction extends TransactionBuilder.For[CancelSponsorshipTransaction] {

  override def typeId: Byte                 = 19
  override def supportedVersions: Set[Byte] = SponsorshipTransactionBase.supportedVersions

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount): TransactionT =
    tx.copy(proofs = Proofs(crypto.sign(signer, tx.bodyBytes())))

  object SerializerV1 extends SponsorshipSerializerV1[CancelSponsorshipTransaction] {
    def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Try {
      (for {
        parsed <- parseBase(bytes)
        (chainId, timestamp, sender, fee, recipient, proofs) = parsed
        tx <- create(version, Some(chainId), timestamp, sender, fee, recipient, None, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => SerializerV1
    case _ => UnknownSerializer
  }

  implicit object Validator extends SponsorshipTransactionBase.Validator[TransactionT]

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: Address,
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    CancelSponsorshipTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, recipient, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: Address,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, recipient, None, Proofs.empty).signWith(signer)

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 fee: Long,
                 recipient: Address): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, fee, recipient, sender)
}
