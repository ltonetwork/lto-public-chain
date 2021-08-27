package com.ltonetwork.transaction.transfer

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import com.ltonetwork.account.KeyTypes.ED25519
import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.Validation
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.ltonetwork.utils.Base58
import monix.eval.Coeval
import play.api.libs.json.{JsObject, JsValue, Json, OFormat}

import scala.util.{Either, Try}

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
    with Transaction.HardcodedV1 {

  override def builder: TransactionBuilder.For[MassTransferTransaction]      = MassTransferTransaction
  private def serializer: TransactionSerializer.For[MassTransferTransaction] = builder.serializer(version)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(serializer.bodyBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(jsonBase ++ Json.obj(
    "attachment"    -> Base58.encode(attachment),
    "transferCount" -> transfers.size,
    "totalAmount"   -> transfers.map(_.amount).sum,
    "transfers"     -> MassTransferTransaction.toJson(transfers)
  ))

  def compactJson(recipients: Set[Address]): JsObject =
    json() ++ Json.obj("transfers" -> MassTransferTransaction.toJson(transfers.filter(t => recipients.contains(t.address))))
}

object MassTransferTransaction extends TransactionBuilder.For[MassTransferTransaction] {
  import TransactionParser._

  override val typeId: Byte                 = 11
  override val supportedVersions: Set[Byte] = Set(1, 3)

  val MaxTransferCount = 100

  case class Transfer(recipient: String, amount: Long)
  object Transfer {
    implicit val jsonFormat: OFormat[Transfer] = Json.format[Transfer]
  }

  case class ParsedTransfer(address: Address, amount: Long)

  implicit def sign(tx: TransactionT, signer: PrivateKeyAccount, sponsor: Option[PublicKeyAccount]): TransactionT =
    tx.copy(proofs = tx.proofs + signer.sign(tx.bodyBytes()), sponsor = sponsor.otherwise(tx.sponsor))

  implicit object Validator extends TxValidator[TransactionT] {
    private def validateTotalAmount(tx: TransactionT): ValidatedNel[ValidationError, Unit] =
      Try {
        tx.transfers.map(_.amount).fold(tx.fee)(Math.addExact)
      }.fold(
        _ => ValidationError.OverflowError.invalidNel,
        _ => ().validNel
      )

    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, (), ValidationError.WrongChainId(chainId)),
        Validated.condNel(transfers.lengthCompare(MaxTransferCount) <= 0,
                          (),
                          ValidationError.GenericError(s"Number of transfers is greater than $MaxTransferCount")),
        Validated.condNel(!transfers.exists(_.amount < 0), (), ValidationError.GenericError("One of the transfers has negative amount")),
        validateTotalAmount(tx),
        Validated.condNel(attachment.length <= TransferTransaction.MaxAttachmentSize, (), ValidationError.TooBigArray),
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

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => MassTransferSerializerV1
    case 3 => MassTransferSerializerV3
    case _ => UnknownSerializer
  }

  override def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] =
    if (bytes(0) != 0) OneVersion(typeId, 1).parseHeader(bytes)
    else MultipleVersions(typeId, supportedVersions).parseHeader(bytes)

  def create(version: Byte,
             chainId: Option[Byte],
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             transfers: List[ParsedTransfer],
             attachment: Array[Byte],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    MassTransferTransaction(version, chainId.getOrElse(networkByte), timestamp, sender, fee, transfers, attachment, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PrivateKeyAccount,
             fee: Long,
             transfers: List[ParsedTransfer],
             attachment: Array[Byte]): Either[ValidationError, TransactionT] =
    create(version, None, timestamp, sender, fee, transfers, attachment, None, Proofs.empty).signWith(sender)

  def parseTransfersList(transfers: List[Transfer]): Validation[List[ParsedTransfer]] = {
    transfers.traverse {
      case Transfer(recipient, amount) =>
        Address.fromString(recipient).map(ParsedTransfer(_, amount))
    }
  }

  def toJson(transfers: List[ParsedTransfer]): JsValue =
    Json.toJson(transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) })
}
