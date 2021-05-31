package com.ltonetwork.transaction.transfer

import cats.implicits._
import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import com.ltonetwork.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.JsObject
import scala.util.Try

case class TransferTransaction private(version: Byte,
                                       chainId: Byte,
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

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => TransferSerializerV1
    case 2 => TransferSerializerV2
    case _ => UnknownSerializer
  }

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
        Validated.condNel(chainId != networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(attachment.length > TransferTransaction.MaxAttachmentSize, None, ValidationError.TooBigArray),
        Validated.condNel(fee <= 0, None, ValidationError.InsufficientFee()),
        validateSum(Seq(amount, fee)),
        Validated.condNel(sponsor.isDefined && version < 3, None, ValidationError.SponsoredTxNotSupported(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] =
    if (bytes(0) != 0) serializer(1).parseBytes(1, bytes)
    else super.parseBytes(bytes)

  def create(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: AddressOrAlias,
             amount: Long,
             attachment: Array[Byte],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    TransferTransaction(version, networkByte, timestamp, sender, fee, recipient, amount, attachment, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             recipient: AddressOrAlias,
             amount: Long,
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, timestamp, sender, fee, recipient, amount, attachment, None, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }

  def selfSigned(version: Byte,
                 timestamp: Long,
                 fee: Long,
                 sender: PrivateKeyAccount,
                 recipient: AddressOrAlias,
                 amount: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, fee, recipient, amount, attachment, sender)
}
