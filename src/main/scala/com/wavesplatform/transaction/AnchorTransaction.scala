package com.wavesplatform.transaction

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.crypto
import com.wavesplatform.state._
import monix.eval.Coeval
import play.api.libs.json._
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

case class AnchorTransaction private (version: Byte, sender: PublicKeyAccount, data: List[ByteStr], fee: Long, timestamp: Long, proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = AnchorTransaction
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      Shorts.toByteArray(data.size.toShort),
      data.flatMap(_.arr).toArray,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  implicit val dataItemFormat: Format[DataEntry[_]] = DataEntry.Format

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version" -> version,
      "anchors" -> Json.toJson(data)
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
}

object AnchorTransaction extends TransactionParserFor[AnchorTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 15
  override val supportedVersions: Set[Byte] = Set(1)

  val EntryLength   = 64
  val MaxBytes      = 150 * 1024
  val MaxEntryCount = 100

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val p0     = KeyLength
      val sender = PublicKeyAccount(bytes.slice(0, p0))

      val entryCount = Shorts.fromByteArray(bytes.drop(p0))
      val p01        = p0 + 2
      val p1         = p01 + entryCount * EntryLength
      val (entries) =
        Range(0, entryCount)
          .foldLeft(List.empty[ByteStr]) {
            case (l, i) => ByteStr(bytes.slice(p01 + i * EntryLength, p01 + (i + 1) * EntryLength)) +: l
          }
          .reverse
      val timestamp = Longs.fromByteArray(bytes.drop(p1))
      val feeAmount = Longs.fromByteArray(bytes.drop(p1 + 8))
      val txEi = for {
        proofs <- Proofs.fromBytes(bytes.drop(p1 + 16))
        tx     <- create(version, sender, entries, feeAmount, timestamp, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             data: List[ByteStr],
             feeAmount: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (data.lengthCompare(MaxEntryCount) > 0) {
      Left(ValidationError.TooBigArray)
    } else if (data.distinct.lengthCompare(data.size) < 0) {
      Left(ValidationError.GenericError("Duplicate in one tx found"))
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      val tx = AnchorTransaction(version, sender, data, feeAmount, timestamp, proofs)
      Either.cond(tx.bytes().length <= MaxBytes, tx, ValidationError.TooBigArray)
    }
  }

  def signed(version: Byte,
             sender: PublicKeyAccount,
             data: List[ByteStr],
             feeAmount: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(version, sender, data, feeAmount, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 data: List[ByteStr],
                 feeAmount: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(version, sender, data, feeAmount, timestamp, sender)
  }
}
