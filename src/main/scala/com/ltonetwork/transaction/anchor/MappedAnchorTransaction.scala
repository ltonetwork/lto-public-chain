package com.ltonetwork.transaction.anchor

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.utils.base58Length
import com.ltonetwork.transaction._
import monix.eval.Coeval
import play.api.libs.json._

case class MappedAnchorTransaction private(version: Byte,
                                           chainId: Byte,
                                           timestamp: Long,
                                           sender: PublicKeyAccount,
                                           fee: Long,
                                           anchors: Map[ByteStr, ByteStr],
                                           sponsor: Option[PublicKeyAccount],
                                           proofs: Proofs)
  extends Transaction {

  override def builder: TransactionBuilder.For[MappedAnchorTransaction]      = MappedAnchorTransaction
  private def serializer: TransactionSerializer.For[MappedAnchorTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(
    jsonBase ++ Json.obj("anchors" -> Json.toJson(anchors.map { case (k, v) => (k.base58, v.base58) }))
  )
}

object MappedAnchorTransaction extends TransactionBuilder.For[MappedAnchorTransaction] {

  override val typeId: Byte                 = 22
  override val supportedVersions: Set[Byte] = Set(3)

  val MaxEntryLength: Int = 80
  val MaxEntryCount: Int  = 100

  val MaxAnchorStringSize: Int = base58Length(MaxEntryLength)

  case class Transfer(recipient: String, amount: Long)
  object Transfer {
    implicit val jsonFormat: OFormat[Transfer] = Json.format[Transfer]
  }

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(anchors.toSeq.lengthCompare(MaxEntryCount) <= 0, (), ValidationError.TooBigArray),
        Validated.condNel(tx.anchors.forall { case (k, _) => k.arr.length <= MaxEntryLength },
          (),
          ValidationError.GenericError(s"Key should be max $MaxEntryLength bytes")),
        Validated.condNel(tx.anchors.forall { case (_, a) => a.arr.length <= MaxEntryLength },
          (),
          ValidationError.GenericError(s"Anchor should be max $MaxEntryLength bytes")),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 3 => MappedAnchorSerializerV3
    case _ => UnknownSerializer
  }

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             anchors: Map[ByteStr, ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    MappedAnchorTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, anchors, sponsor, proofs).validatedEither

  def signed(version: Byte, timestamp: Long, sender: PrivateKeyAccount, fee: Long, anchors: Map[ByteStr, ByteStr]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, anchors, None, Proofs.empty).signWith(sender)
}
