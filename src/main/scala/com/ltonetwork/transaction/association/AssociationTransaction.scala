package com.ltonetwork.transaction.association

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{Address, AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.{Proofs, Transaction, TransactionBuilder, TransactionSerializer, TxValidator, ValidationError}
import monix.eval.Coeval
import play.api.libs.json._


case class AssociationTransaction private (version: Byte,
                                           chainId: Byte,
                                           timestamp: Long,
                                           sender: PublicKeyAccount,
                                           fee: Long,
                                           assocType: Int,
                                           recipient: AddressOrAlias,
                                           expires: Option[Long],
                                           hash: Option[ByteStr],
                                           sponsor: Option[PublicKeyAccount],
                                           proofs: Proofs)
    extends Transaction {

  override val builder: TransactionBuilder.For[AssociationTransaction] = AssociationTransaction
  private val serializer: TransactionSerializer.For[AssociationTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = serializer.bodyBytes(this)
  override val json: Coeval[JsObject] = serializer.toJson(this)
}

object AssociationTransaction extends TransactionBuilder.For[AssociationTransaction] {

  override def typeId: Byte                 = 16
  override def supportedVersions: Set[Byte] = Set(1: Byte)

  val MaxHashLength: Int    = 64
  val StringHashLength: Int = com.ltonetwork.utils.base58Length(AssociationTransaction.MaxHashLength)

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount): TransactionT =
    tx.copy(proofs = Proofs(crypto.sign(signer, tx.bodyBytes())))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId != networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(hash.exists(_.arr.length > MaxHashLength), None, ValidationError.GenericError(s"Hash length must be <= $MaxHashLength bytes")),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(expires.isDefined && version < 3, None, ValidationError.UnsupportedFeature(s"Association expiry is not supported for tx v$version")),
        Validated.condNel(sponsor.isDefined && version < 3, None, ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => AssociationSerializerV1
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             assocType: Int,
             recipient: Address,
             expires: Option[Long],
             hash: Option[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    AssociationTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, assocType, recipient, expires, hash, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             assocType: Int,
             recipient: Address,
             expires: Option[Long],
             hash: Option[ByteStr],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, assocType, recipient, expires, hash, None, Proofs.empty).signWith(signer)

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 fee: Long,
                 assocType: Int,
                 recipient: Address,
                 expires: Option[Long],
                 hash: Option[ByteStr]): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, fee, assocType, recipient, expires, hash, sender)
}
