package com.ltonetwork.transaction.claim

import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.account.KeyTypes.ED25519
import com.ltonetwork.state._
import com.ltonetwork.utils.base58Length
import com.ltonetwork.transaction._
import monix.eval.Coeval
import play.api.libs.json._

case class ClaimTransaction private (version: Byte,
                                      chainId: Byte,
                                      timestamp: Long,
                                      sender: PublicKeyAccount,
                                      fee: Long,
                                      anchors: List[ByteStr],
                                      sponsor: Option[PublicKeyAccount],
                                      proofs: Proofs)
  extends Transaction {

  override def builder: TransactionBuilder.For[ClaimTransaction]      = ClaimTransaction
  private def serializer: TransactionSerializer.For[ClaimTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(jsonBase ++ Json.obj("anchors" -> Json.toJson(anchors)))
}

object ClaimTransaction extends TransactionBuilder.For[ClaimTransaction] {

  override val typeId: Byte                 = 20
  override val supportedVersions: Set[Byte] = Set(3)

  val EntryLength: List[Int] = List(16, 20, 32, 48, 64)
  val NewMaxEntryLength: Int = 64
  val MaxBytes: Int          = 150 * 1024
  val MaxEntryCount: Int     = 100

  val MaxClaimStringSize: Int = base58Length(EntryLength.last)

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(anchors.lengthCompare(MaxEntryCount) <= 0, (), ValidationError.TooBigArray),
        Validated.condNel(anchors.forall(a => EntryLength.contains(a.arr.length)),
          (),
          ValidationError.GenericError(s"Claim can only be of length $EntryLength Bytes")),
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
    case 3 => ClaimSerializerV3
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
    ClaimTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, anchors, sponsor, proofs).validatedEither

  def signed(version: Byte, timestamp: Long, sender: PrivateKeyAccount, fee: Long, anchors: List[ByteStr]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, anchors, None, Proofs.empty).signWith(sender)
}
