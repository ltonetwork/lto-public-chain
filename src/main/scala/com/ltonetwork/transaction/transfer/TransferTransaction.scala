package com.ltonetwork.transaction.transfer

import cats.implicits._
import cats.data.{Validated, ValidatedNel}
import com.google.common.primitives.Bytes
import com.ltonetwork.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.validation.{validateAmount, validateAttachment, validateFee, validateSum}
import com.ltonetwork.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.JsObject
import scorex.crypto.signatures.Curve25519._

import scala.util.{Failure, Success, Try}

case class TransferTransaction private(version: Byte,
                                       timestamp: Long,
                                       sender: PublicKeyAccount,
                                       fee: Long,
                                       recipient: AddressOrAlias,
                                       amount: Long,
                                       attachment: Array[Byte],
                                       sponsor: Option[PublicKeyAccount],
                                       proofs: Proofs)
    extends Transaction
    with Transaction.NoBytePrefixV1 {

  override val builder: TransactionBuilder.For[TransferTransaction] = TransferTransaction
  private val serializer: TransactionSerializer.For[TransferTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = serializer.bodyBytes(this)
  override val json: Coeval[JsObject] = serializer.toJson(this)
}

object TransferTransaction extends TransactionBuilder.For[TransferTransaction] {

  override val typeId: Byte = 4
  override val supportedVersions: Set[Byte] = Set(1, 2)

  val MaxAttachmentSize: Int       = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  implicit object Validator extends TxValidator[TransactionT] {
    private def validateSum(amounts: Seq[Long]): ValidatedNel[ValidationError, None.type] =
      Try(amounts.tail.fold(amounts.head)(Math.addExact))
        .fold (
          _ => ValidationError.OverflowError.invalidNel,
          _ => None.validNel
        )

    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(attachment.length > TransferTransaction.MaxAttachmentSize, None, ValidationError.TooBigArray),
        Validated.condNel(fee <= 0, None, ValidationError.InsufficientFee()),
        validateSum(Seq(amount, fee)),
        Validated.condNel(sponsor.isDefined && version < 3, None, ValidationError.SponsoredTxNotSupported(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

  def create(sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             fee: Long,
             attachment: Array[Byte],
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    TransferTransaction
      .validate(amount, fee, attachment)
      .map(_ => TransferTransaction(sender, recipient, amount, timestamp, fee, attachment, signature))
  }

  def signed(sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             fee: Long,
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, recipient, amount, timestamp, fee, attachment, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(sender: PrivateKeyAccount,
                 recipient: AddressOrAlias,
                 amount: Long,
                 timestamp: Long,
                 fee: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    signed(sender, recipient, amount, timestamp, fee, attachment, sender)
  }
}
