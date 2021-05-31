package com.ltonetwork.transaction.transfer

import cats.implicits._
import cats.data.{Validated, ValidatedNel}
import com.ltonetwork.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.Validation
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.transfer.MassTransferTransaction.ParsedTransfer
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json, OFormat}

import scala.util.{Either, Failure, Success, Try}

case class MassTransferTransaction private (version: Byte,
                                            chainId: Byte,
                                            timestamp: Long,
                                            sender: PublicKeyAccount,
                                            fee: Long,
                                            transfers: List[ParsedTransfer],
                                            attachment: Array[Byte],
                                            sponsor: Option[PublicKeyAccount],
                                            proofs: Proofs)
    extends Transaction
    with Transaction.NoBytePrefixV1 {

  override val builder: TransactionBuilder.For[MassTransferTransaction] = MassTransferTransaction
  private val serializer: TransactionSerializer.For[MassTransferTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = serializer.bodyBytes(this)
  override val json: Coeval[JsObject] = serializer.toJson(this)
}

object MassTransferTransaction extends TransactionBuilder.For[MassTransferTransaction] {

  override val typeId: Byte  = 11
  override val supportedVersions: Set[Byte] = Set(1)

  val MaxTransferCount = 100

  case class Transfer(recipient: String, amount: Long)
  object Transfer {
    implicit val jsonFormat: OFormat[Transfer] = Json.format[Transfer]
  }

  case class ParsedTransfer(address: AddressOrAlias, amount: Long)

  implicit object Validator extends TxValidator[TransactionT] {
    private def validateTotalAmount(tx: TransactionT): ValidatedNel[ValidationError, None.type] =
      Try {
        tx.transfers.map(_.amount).fold(tx.fee)(Math.addExact)
      }.fold (
        _ => ValidationError.OverflowError.invalidNel,
        _ => None.validNel
      )

    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId != networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(transfers.lengthCompare(MaxTransferCount) > 0, None, ValidationError.GenericError(s"Number of transfers is greater than $MaxTransferCount")),
        Validated.condNel(transfers.exists(_.amount < 0), None, ValidationError.GenericError("One of the transfers has negative amount")),
        validateTotalAmount(tx),
        Validated.condNel(attachment.length > TransferTransaction.MaxAttachmentSize, None, ValidationError.TooBigArray),
        Validated.condNel(fee <= 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isDefined && version < 3, None, ValidationError.SponsoredTxNotSupported(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => MassTransferSerializerV1
    case _ => UnknownSerializer
  }

  override protected def parseBytes(bytes: Array[Byte]): Try[TransactionT] = {
    Try {
      val txEi = for {
        (version, end) <- TransactionParser.MultipleVersions(typeId, supportedVersions).parseHeader(bytes)
        tx             <- serializer(version).parseBytes(version, bytes.drop(end))
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  def create(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             transfers: List[ParsedTransfer],
             attachment: Array[Byte],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    MassTransferTransaction(version, networkByte, timestamp, sender, fee, transfers, attachment, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             transfers: List[ParsedTransfer],
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, timestamp, sender, fee, transfers, attachment, None, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 fee: Long,
                 transfers: List[ParsedTransfer],
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, fee, transfers, attachment, sender)

  def parseTransfersList(transfers: List[Transfer]): Validation[List[ParsedTransfer]] = {
    transfers.traverse {
      case Transfer(recipient, amount) =>
        AddressOrAlias.fromString(recipient).map(ParsedTransfer(_, amount))
    }
  }
}
