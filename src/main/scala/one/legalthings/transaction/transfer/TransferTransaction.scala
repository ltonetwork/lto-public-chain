package one.legalthings.transaction.transfer

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import one.legalthings.account.{AddressOrAlias, PublicKeyAccount}
import one.legalthings.serialization.Deser
import one.legalthings.transaction._
import one.legalthings.transaction.validation._
import one.legalthings.utils.{Base58, base58Length}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519._

trait TransferTransaction extends ProvenTransaction with VersionedTransaction {
  def recipient: AddressOrAlias
  def amount: Long
  def fee: Long
  def attachment: Array[Byte]
  def version: Byte

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "recipient"  -> recipient.stringRepr,
      "amount"     -> amount,
      "attachment" -> Base58.encode(attachment)
    ))

  final protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes    = Longs.toByteArray(amount)
    val feeBytes       = Longs.toByteArray(fee)

    Bytes.concat(
      sender.publicKey,
      timestampBytes,
      amountBytes,
      feeBytes,
      recipient.bytes.arr,
      Deser.serializeArray(attachment)
    )
  }
}

object TransferTransaction {

  val MaxAttachmentSize            = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  def validate(amount: Long, feeAmount: Long, attachment: Array[Byte]): Either[ValidationError, Unit] = {
    (
      validateAmount(amount, "waves"),
      validateFee(feeAmount),
      validateAttachment(attachment),
      validateSum(Seq(amount, feeAmount))
    ).mapN { case _ => () }
      .toEither
      .leftMap(_.head)
  }

  def parseBase(bytes: Array[Byte], start: Int) = {
    val sender    = PublicKeyAccount(bytes.slice(start, start + KeyLength))
    val s1        = start + KeyLength
    val timestamp = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
    val amount    = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
    val feeAmount = Longs.fromByteArray(bytes.slice(s1 + 16, s1 + 24))
    for {
      recRes <- AddressOrAlias.fromBytes(bytes, s1 + 24)
      (recipient, recipientEnd) = recRes
      (attachment, end)         = Deser.parseArraySize(bytes, recipientEnd)
    } yield (sender, timestamp, amount, feeAmount, recipient, attachment, end)

  }

}
