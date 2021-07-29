package com.ltonetwork.transaction.transfer

import cats.data.{Validated, ValidatedNel}
import com.google.common.primitives.Bytes
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.TransactionParser.{HardcodedVersion1, MultipleVersions}
import com.ltonetwork.transaction.Transaction.{HardcodedV1, SigProofsSwitch}
import com.ltonetwork.transaction._
import com.ltonetwork.utils.{Base58, base58Length}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class TransferTransaction private (version: Byte,
                                        chainId: Byte,
                                        timestamp: Long,
                                        sender: PublicKeyAccount,
                                        fee: Long,
                                        recipient: Address,
                                        amount: Long,
                                        attachment: Array[Byte],
                                        sponsor: Option[PublicKeyAccount],
                                        proofs: Proofs)
    extends Transaction
    with HardcodedV1
    with SigProofsSwitch {

  override def builder: TransactionBuilder.For[TransferTransaction]      = TransferTransaction
  private def serializer: TransactionSerializer.For[TransferTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(jsonBase ++ Json.obj(
    "recipient"  -> recipient.stringRepr,
    "amount"     -> amount,
    "attachment" -> Base58.encode(attachment)
  ))

  // Special case for transfer tx v1: signature is prepended instead of appended
  override protected def prefixByte: Coeval[Array[Byte]] =
    Coeval.evalOnce(version match {
      case 1 if proofs.isEmpty => throw new Exception("Transaction not signed")
      case 1                   => Bytes.concat(Array(builder.typeId), proofs.toSignature.arr)
      case _                   => Array(0: Byte)
    })

  override protected def footerBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (version == 1) Array.emptyByteArray
    else super[Transaction].footerBytes()
  )
}

object TransferTransaction extends TransactionBuilder.For[TransferTransaction] {

  override val typeId: Byte                 = 4
  override val supportedVersions: Set[Byte] = Set(1, 2, 3)

  val MaxAttachmentSize: Int       = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => TransferSerializerV1
    case 2 => TransferSerializerV2
    case 3 => TransferSerializerV3
    case _ => UnknownSerializer
  }

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(attachment.length <= TransferTransaction.MaxAttachmentSize, None, ValidationError.TooBigArray),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(Try(Math.addExact(amount, fee)).isSuccess, None, ValidationError.OverflowError),
        Validated.condNel(sponsor.isEmpty || version >= 3,
                          None,
                          ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
        Validated.condNel(proofs.length <= 1 || version > 1, None, ValidationError.UnsupportedFeature(s"Multiple proofs not supported for tx v1")),
      )
    }
  }

  override def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] =
    if (bytes(0) != 0) HardcodedVersion1(typeId).parseHeader(bytes)
    else MultipleVersions(typeId, supportedVersions).parseHeader(bytes)

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: Address,
             amount: Long,
             attachment: Array[Byte],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    TransferTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, recipient, amount, attachment, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PrivateKeyAccount,
             fee: Long,
             recipient: Address,
             amount: Long,
             attachment: Array[Byte]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, recipient, amount, attachment, None, Proofs.empty).signWith(sender)
}
