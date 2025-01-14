package com.ltonetwork.transaction.association

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.KeyTypes.ED25519
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction.{Proofs, TransactionBuilder, TransactionSerializer, TxValidator, ValidationError}
import monix.eval.Coeval
import play.api.libs.json._

case class RevokeAssociationTransaction private (version: Byte,
                                                 chainId: Byte,
                                                 timestamp: Long,
                                                 sender: PublicKeyAccount,
                                                 fee: Long,
                                                 assocType: Long,
                                                 recipient: Address,
                                                 subject: Option[ByteStr],
                                                 sponsor: Option[PublicKeyAccount],
                                                 proofs: Proofs)
    extends AssociationTransaction {

  override def builder: TransactionBuilder.For[RevokeAssociationTransaction]      = RevokeAssociationTransaction
  private def serializer: TransactionSerializer.For[RevokeAssociationTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))

  val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase ++ (
      Json.obj(
        "associationType" -> assocType,
        "recipient"       -> recipient.stringRepr,
      )
        ++ subject.fold(Json.obj())(v => Json.obj("subject" -> v.base58))
    ))
}

object RevokeAssociationTransaction extends TransactionBuilder.For[RevokeAssociationTransaction] {

  override def typeId: Byte                 = 17
  override def supportedVersions: Set[Byte] = Set(1, 3, 4)

  val MaxSubjectLength: Int = IssueAssociationTransaction.MaxSubjectLength

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(version >= 4 || assocType.isValidInt, (), ValidationError.GenericError(s"Association type must be a valid integer for v$version")),
        Validated.condNel(version < 3 || !subject.exists(_.arr.length == 0), (), ValidationError.GenericError("Subject length must not be 0 bytes")),
        Validated.condNel(!subject.exists(_.arr.length > MaxSubjectLength),
                          (),
                          ValidationError.GenericError(s"Subject length must be <= ${MaxSubjectLength} bytes")),
        Validated.condNel(fee > 0, (), ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isEmpty || version >= 3,
                          (),
                          ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
        Validated.condNel(sender.keyType == ED25519 || version >= 3,
                          None,
                          ValidationError.UnsupportedFeature(s"Sender key type ${sender.keyType} not supported for tx v$version"))
      )
    }
  }

  object SerializerV1 extends AssociationSerializerV1[RevokeAssociationTransaction] {
    protected val createTx = (version, chainId, timestamp, sender, fee, assocType, recipient, subject, proofs) =>
      create(version, Some(chainId), timestamp, sender, fee, assocType, recipient, subject, None, proofs)
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => SerializerV1
    case 3 => RevokeAssociationSerializerV3
    case 4 => RevokeAssociationSerializerV4
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             assocType: Long,
             recipient: Address,
             subject: Option[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    RevokeAssociationTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, assocType, recipient, subject, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PrivateKeyAccount,
             fee: Long,
             assocType: Long,
             recipient: Address,
             subject: Option[ByteStr]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, assocType, recipient, subject, None, Proofs.empty).signWith(sender)
}
