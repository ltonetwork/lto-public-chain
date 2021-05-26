package com.ltonetwork.transaction.anchor

import cats.data.{Validated, ValidatedNel}
import com.google.common.primitives.Bytes
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import monix.eval.Coeval
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

case class AnchorTransaction private(version: Byte,
                                     timestamp: Long,
                                     sender: PublicKeyAccount,
                                     fee: Long,
                                     anchors: List[ByteStr],
                                     sponsor: Option[PublicKeyAccount],
                                     proofs: Proofs)
  extends Transaction {

  override val builder: TransactionBuilder.For[AnchorTransaction] = AnchorTransaction
  private val serializer: TransactionSerializer.For[AnchorTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = serializer.bodyBytes(this)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), footerBytes()))
  override val json: Coeval[JsObject] = serializer.json(this)
}

object AnchorTransaction extends TransactionBuilder.For[AnchorTransaction] {

  override val typeId: Byte                 = 15
  override val supportedVersions: Set[Byte] = Set(1, 3)

  val EntryLength: List[Int] = List(16, 20, 32, 48, 64)
  val NewMaxEntryLength: Int = 64
  val MaxBytes: Int          = 150 * 1024
  val MaxEntryCount: Int     = 100

  implicit object Validator extends TxValidator[TransactionT] {
    def validate(tx: TransactionT): ValidatedNel[ValidationError, TransactionT] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(anchors.lengthCompare(MaxEntryCount) > 0, None, ValidationError.TooBigArray),
        Validated.condNel(anchors.exists(a => !EntryLength.contains(a.arr.length)), None, ValidationError.GenericError(s"Anchor can only be of length $EntryLength Bytes")),
        Validated.condNel(anchors.distinct.lengthCompare(anchors.size) < 0, None, ValidationError.GenericError("Duplicate anchor in one tx found")),
        Validated.condNel(fee <= 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isDefined && version < 3, None, ValidationError.SponsoredTxNotSupported(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[TransactionT] = version match {
    case 1 => AnchorSerializerV1
    case 3 => AnchorSerializerV3
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
             anchors: List[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] =
    AnchorTransaction(version, timestamp, sender, fee, anchors, sponsor, proofs).validatedEither

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             fee: Long,
             data: List[ByteStr],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, timestamp, sender, fee, data, None, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 fee: Long,
                 data: List[ByteStr]): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, fee, data, sender)
}
