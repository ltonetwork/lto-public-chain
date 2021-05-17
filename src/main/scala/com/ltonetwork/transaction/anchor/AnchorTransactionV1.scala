package com.ltonetwork.transaction.anchor

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction._
import monix.eval.Coeval
import play.api.libs.json._
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

case class AnchorTransactionV1 private(version: Byte, sender: PublicKeyAccount, anchors: List[ByteStr], fee: Long, timestamp: Long, proofs: Proofs)
    extends AnchorTransaction {

  override val builder: TransactionParser = AnchorTransactionV1
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      Deser.serializeArrays(anchors.map(_.arr)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version" -> version,
      "anchors" -> Json.toJson(anchors)
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
}

object AnchorTransactionV1 extends TransactionParserFor[AnchorTransactionV1] with AnchorTransactionParser {

  override val supportedVersions: Set[Byte] = Set(1)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val p0     = KeyLength
      val sender = PublicKeyAccount(bytes.slice(0, p0))

      val txEi = for {
        r <- Try(Deser.parseArraysPos(bytes.drop(p0))).toEither.left.map(x => GenericError(x.toString))
        arrays    = r._1
        pos       = r._2
        timestamp = Longs.fromByteArray(bytes.drop(p0 + pos))
        feeAmount = Longs.fromByteArray(bytes.drop(p0 + pos + 8))

        proofs <- Proofs.fromBytes(bytes.drop(p0 + pos + 16))
        tx     <- create(version, sender, arrays.map(ByteStr(_)).toList, feeAmount, timestamp, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             data: List[ByteStr],
             feeAmount: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    validate(version, data, feeAmount).right.map { _ =>
      AnchorTransactionV1(version, sender, data, feeAmount, timestamp, proofs)
    }.right.flatMap(tx => Either.cond(tx.bytes().length <= MaxBytes, tx, ValidationError.TooBigArray))
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
