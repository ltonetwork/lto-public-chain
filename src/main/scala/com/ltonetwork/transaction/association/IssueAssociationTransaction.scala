package com.ltonetwork.transaction.association

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction.{Proofs, TransactionBuilder, TransactionSerializer, TxValidator, ValidationError}
import monix.eval.Coeval
import play.api.libs.json._

case class IssueAssociationTransaction private (version: Byte,
                                                chainId: Byte,
                                                timestamp: Long,
                                                sender: PublicKeyAccount,
                                                fee: Long,
                                                recipient: Address,
                                                assocType: Int,
                                                expires: Option[Long],
                                                hash: Option[ByteStr],
                                                sponsor: Option[PublicKeyAccount],
                                                proofs: Proofs)
    extends AssociationTransaction {

  override def builder: TransactionBuilder.For[IssueAssociationTransaction]      = IssueAssociationTransaction
  private def serializer: TransactionSerializer.For[IssueAssociationTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))

  val json: Coeval[JsObject] = Coeval.evalOnce(jsonBase ++
    Json.obj(
      "associationType" -> assocType,
      "party"           -> recipient.stringRepr,
    ) ++
    expires.fold(Json.obj())(e => Json.obj("expires" -> e)) ++
    hash.fold(Json.obj())(h => Json.obj("hash" -> h.base58))
  )
}

object IssueAssociationTransaction extends TransactionBuilder.For[IssueAssociationTransaction] {

  override def typeId: Byte                 = 16
  override def supportedVersions: Set[Byte] = Set(1: Byte)

  val MaxHashLength: Int    = 64
  val StringHashLength: Int = com.ltonetwork.utils.base58Length(IssueAssociationTransaction.MaxHashLength)

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(!hash.exists(_.arr.length > MaxHashLength),
                          None,
                          ValidationError.GenericError(s"Hash length must be <= $MaxHashLength bytes")),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(expires.isEmpty || version >= 3,
                          None,
                          ValidationError.UnsupportedFeature(s"Association expiry is not supported for tx v$version")),
        Validated.condNel(sponsor.isEmpty || version >= 3,
                          None,
                          ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

  object SerializerV1 extends AssociationSerializerV1[IssueAssociationTransaction] {
    protected def createTx(version: Byte,
                           chainId: Byte,
                           timestamp: Long,
                           sender: PublicKeyAccount,
                           fee: Long,
                           recipient: Address,
                           assocType: Int,
                           hash: Option[ByteStr],
                           proofs: Proofs): Either[ValidationError, TransactionT] =
      create(version, Some(chainId), timestamp, sender, fee, recipient, assocType, None, hash, None, proofs)
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => SerializerV1
    case 3 => IssueAssociationSerializerV3
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: Address,
             assocType: Int,
             expires: Option[Long],
             hash: Option[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    IssueAssociationTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, recipient, assocType, expires, hash, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: Address,
             assocType: Int,
             expires: Option[Long],
             hash: Option[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, recipient, assocType, expires, hash, sponsor, proofs).signWith(signer)

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 fee: Long,
                 recipient: Address,
                 assocType: Int,
                 expires: Option[Long],
                 hash: Option[ByteStr]): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, fee, recipient, assocType, expires, hash, None, Proofs.empty, sender)
}
