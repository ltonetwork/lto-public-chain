package com.ltonetwork.transaction.association

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.{Proofs, Transaction, TransactionBuilder, TransactionSerializer, TxValidator, ValidationError}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.{Failure, Success, Try}

case class RevokeAssociationTransaction private (version: Byte,
                                                 chainId: Byte,
                                                 timestamp: Long,
                                                 sender: PublicKeyAccount,
                                                 fee: Long,
                                                 recipient: Address,
                                                 assocType: Int,
                                                 hash: Option[ByteStr],
                                                 sponsor: Option[PublicKeyAccount],
                                                 proofs: Proofs)
    extends AssociationTransaction {

  override def builder: TransactionBuilder.For[RevokeAssociationTransaction]      = RevokeAssociationTransaction
  private def serializer: TransactionSerializer.For[RevokeAssociationTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(serializer.toJson(this))
}

object RevokeAssociationTransaction extends TransactionBuilder.For[RevokeAssociationTransaction] {

  override def typeId: Byte                 = 17
  override def supportedVersions: Set[Byte] = Set(1: Byte)

  val MaxHashLength: Int    = IssueAssociationTransaction.MaxHashLength
  val StringHashLength: Int = IssueAssociationTransaction.StringHashLength

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount): TransactionT =
    tx.copy(proofs = Proofs(crypto.sign(signer, tx.bodyBytes())))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(!hash.exists(_.arr.length > MaxHashLength),
                          None,
                          ValidationError.GenericError(s"Hash length must be <= ${MaxHashLength} bytes")),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isEmpty || version >= 3,
                          None,
                          ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

  object SerializerV1 extends AssociationSerializerV1[RevokeAssociationTransaction] {
    override def parseBytes(version: Byte, bytes: Array[Byte]): Try[RevokeAssociationTransaction] =
      Try {
        val chainId = bytes(0)
        (for {
          parsed <- parse(version, bytes)
          (version, timestamp, sender, fee, recipient, assocType, hashOpt, proofs) = parsed
          tx <- create(version, Some(chainId), timestamp, sender, fee, recipient, assocType, hashOpt, None, proofs)
        } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
      }.flatten
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => SerializerV1
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: Address,
             assocType: Int,
             hash: Option[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, RevokeAssociationTransaction] =
    RevokeAssociationTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, recipient, assocType, hash, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: Address,
             assocType: Int,
             hash: Option[ByteStr],
             signer: PrivateKeyAccount): Either[ValidationError, RevokeAssociationTransaction] =
    create(version, None, timestamp, sender, fee, recipient, assocType, hash, None, Proofs.empty).signWith(signer)

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 fee: Long,
                 recipient: Address,
                 assocType: Int,
                 hash: Option[ByteStr]): Either[ValidationError, RevokeAssociationTransaction] =
    signed(version, timestamp, sender, fee, recipient, assocType, hash, sender)
}
