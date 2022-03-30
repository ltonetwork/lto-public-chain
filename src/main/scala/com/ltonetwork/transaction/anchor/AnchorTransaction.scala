package com.ltonetwork.transaction.anchor

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.account.KeyTypes.ED25519
import com.ltonetwork.state._
import com.ltonetwork.utils.base58Length
import com.ltonetwork.transaction._
import monix.eval.Coeval
import play.api.libs.json._

case class AnchorTransaction private (version: Byte,
                                      chainId: Byte,
                                      timestamp: Long,
                                      sender: PublicKeyAccount,
                                      fee: Long,
                                      anchors: List[ByteStr],
                                      sponsor: Option[PublicKeyAccount],
                                      proofs: Proofs)
    extends Transaction {

  override def builder: TransactionBuilder.For[AnchorTransaction]      = AnchorTransaction
  private def serializer: TransactionSerializer.For[AnchorTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(jsonBase ++ Json.obj("anchors" -> Json.toJson(anchors)))
}

object AnchorTransaction extends TransactionBuilder.For[AnchorTransaction] {

  override val typeId: Byte                 = 15
  override val supportedVersions: Set[Byte] = Set(1, 3)

  val MaxEntryLength: Int = 80
  val MaxEntryCount: Int  = 100

  val MaxAnchorStringSize: Int = base58Length(MaxEntryLength)

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(anchors.lengthCompare(MaxEntryCount) <= 0, (), ValidationError.TooBigArray),
        Validated.condNel(
          tx.anchors.forall(a => a.arr.length <= MaxEntryLength),
          (),
          ValidationError.GenericError(s"Anchor should be max $MaxEntryLength bytes")
        ),
        Validated.condNel(anchors.distinct.lengthCompare(anchors.size) == 0, (), ValidationError.GenericError("Duplicate anchor in one tx found")),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isEmpty || version >= 3,
                          (),
                          ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
        Validated.condNel(sender.keyType == ED25519 || version >= 3,
                          None,
                          ValidationError.UnsupportedFeature(s"Sender key type ${sender.keyType} not supported for tx v$version"))
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => AnchorSerializerV1
    case 3 => AnchorSerializerV3
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             anchors: List[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    AnchorTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, anchors, sponsor, proofs).validatedEither

  def signed(version: Byte, timestamp: Long, sender: PrivateKeyAccount, fee: Long, anchors: List[ByteStr]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, anchors, None, Proofs.empty).signWith(sender)
}
