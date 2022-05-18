package com.ltonetwork.transaction.association

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.KeyTypes.ED25519
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
                                                subject: Option[ByteStr],
                                                data: List[DataEntry[_]],
                                                sponsor: Option[PublicKeyAccount],
                                                proofs: Proofs)
    extends AssociationTransaction {

  override def builder: TransactionBuilder.For[IssueAssociationTransaction]      = IssueAssociationTransaction
  private def serializer: TransactionSerializer.For[IssueAssociationTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))

  val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase ++ (
      Json.obj(
        "associationType" -> assocType,
        "recipient"       -> recipient.stringRepr,
      )
        ++ expires.fold(Json.obj())(v => Json.obj("expires" -> v))
        ++ subject.fold(Json.obj())(v => Json.obj("subject" -> v.base58))
        ++ (if (data.nonEmpty) Json.obj() else Json.obj("data" -> data))
    ))
}

object IssueAssociationTransaction extends TransactionBuilder.For[IssueAssociationTransaction] {

  override def typeId: Byte                 = 16
  override def supportedVersions: Set[Byte] = Set(1, 3, 4)

  val MaxSubjectLength: Int = 256
  val MaxBytes: Int         = 10 * 1024
  val MaxEntryCount: Int    = 100

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(version < 3 || !subject.exists(_.arr.length == 0), (), ValidationError.GenericError("Hash length must not be 0 bytes")),
        Validated.condNel(!subject.exists(_.arr.length > MaxSubjectLength),
                          (),
                          ValidationError.GenericError(s"Hash length must be <= $MaxSubjectLength bytes")),
        Validated.condNel(fee > 0, (), ValidationError.InsufficientFee()),
        Validated.condNel(expires.isEmpty || version >= 3,
                          (),
                          ValidationError.UnsupportedFeature(s"Association expiry is not supported for tx v$version")),
        Validated.condNel(data.isEmpty || version >= 4,
                          (),
                          ValidationError.UnsupportedFeature(s"Association data is not supported for tx v$version")),
        Validated.condNel(data.lengthCompare(MaxEntryCount) <= 0 && data.forall(_.valid), (), ValidationError.TooBigArray),
        Validated.condNel(!data.exists(_.key.isEmpty), (), ValidationError.GenericError("Empty key found")),
        Validated.condNel(data.map(_.key).distinct.lengthCompare(data.size) == 0, (), ValidationError.GenericError("Duplicate keys found")),
        Validated.condNel(data.flatMap(_.toBytes).toArray.length <= MaxBytes, (), ValidationError.TooBigArray),
        Validated.condNel(sponsor.isEmpty || version >= 3,
                          (),
                          ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
        Validated.condNel(sender.keyType == ED25519 || version >= 3,
                          None,
                          ValidationError.UnsupportedFeature(s"Sender key type ${sender.keyType} not supported for tx v$version"))
      )
    }
  }

  object SerializerV1 extends AssociationSerializerV1[IssueAssociationTransaction] {
    protected val createTx = (version, chainId, timestamp, sender, fee, recipient, assocType, subject, proofs) =>
      create(version, Some(chainId), timestamp, sender, fee, recipient, assocType, None, subject, List.empty, None, proofs)
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => SerializerV1
    case 3 => IssueAssociationSerializerV3
    case 4 => IssueAssociationSerializerV4
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
             subject: Option[ByteStr],
             data: List[DataEntry[_]],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    IssueAssociationTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, recipient, assocType, expires, subject, data, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PrivateKeyAccount,
             fee: Long,
             recipient: Address,
             assocType: Int,
             expires: Option[Long],
             subject: Option[ByteStr],
             data: List[DataEntry[_]]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, recipient, assocType, expires, subject, data, None, Proofs.empty).signWith(sender)
}
