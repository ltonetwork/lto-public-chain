package com.ltonetwork.transaction.anchor

import com.google.common.primitives.Bytes
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import monix.eval.Coeval
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

case class AnchorTransaction private(version: Byte, timestamp: Long, sender: PublicKeyAccount, fee: Long, anchors: List[ByteStr], sponsor: Option[PublicKeyAccount], proofs: Proofs)
  extends Transaction {

  override val builder: TransactionParser.For[AnchorTransaction] = AnchorTransaction
  private val serializer: TransactionSerializer.For[AnchorTransaction] = builder.serializer(version)

  override val bodyBytes: Coeval[Array[Byte]] = serializer.bodyBytes(this)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), footerBytes()))
  override val json: Coeval[JsObject] = serializer.json(this)
}

object AnchorTransaction extends TransactionParser.For[AnchorTransaction] {
  override val typeId: Byte                 = 15
  override val supportedVersions: Set[Byte] = Set(1, 3)

  val EntryLength: List[Int] = List(16, 20, 32, 48, 64)
  val NewMaxEntryLength: Int = 64
  val MaxBytes: Int          = 150 * 1024
  val MaxEntryCount: Int     = 100

  def validate(version: Byte,
               anchors: List[ByteStr],
               feeAmount: Long,
               sponsor: Option[PublicKeyAccount]): Either[ValidationError, None.type] = {
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (anchors.lengthCompare(MaxEntryCount) > 0) {
      Left(ValidationError.TooBigArray)
    } else if (anchors.exists(a => !EntryLength.contains(a.arr.length))) {
      Left(ValidationError.GenericError(s"Anchor can only be of length $EntryLength Bytes"))
    } else if (anchors.distinct.lengthCompare(anchors.size) < 0) {
      Left(ValidationError.GenericError("Duplicate anchor in one tx found"))
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee())
    } else if (sponsor.isDefined && version < 3) {
      Left(ValidationError.SponsoredTxNotSupported(s"Sponsored transaction not supported for tx v$version"))
    } else {
      Right(None)
    }
  }

  override def serializer(version: Byte): TransactionSerializer.For[AnchorTransaction] = version match {
    case 1 => AnchorSerializerV1
    case 3 => AnchorSerializerV3
    case _ => UnknownSerializer
  }

  override protected def parseBytes(bytes: Array[Byte]): Try[AnchorTransaction] = {
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
             feeAmount: Long,
             anchors: List[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    validate(version, anchors, feeAmount, sponsor).right.map { _ =>
      AnchorTransaction(version, timestamp, sender, feeAmount, anchors, sponsor, proofs)
    }.right.flatMap(tx => Either.cond(tx.bytes().length <= MaxBytes, tx, ValidationError.TooBigArray))
  }

  def signed(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             feeAmount: Long,
             data: List[ByteStr],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(version, timestamp, sender, feeAmount, data, None, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 timestamp: Long,
                 sender: PrivateKeyAccount,
                 feeAmount: Long,
                 data: List[ByteStr]): Either[ValidationError, TransactionT] = {
    signed(version, timestamp, sender, feeAmount, data, sender)
  }

  private object UnknownSerializer extends TransactionSerializer.Unknown[AnchorTransaction]
}
