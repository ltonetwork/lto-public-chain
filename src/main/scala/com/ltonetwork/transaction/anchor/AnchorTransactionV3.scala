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

import scala.util.{Failure, Success, Try}

case class AnchorTransactionV3 private(version: Byte, timestamp: Long, sender: PublicKeyAccount, fee: Long, anchors: List[ByteStr], sponsor: Option[PublicKeyAccount], proofs: Proofs)
  extends AnchorTransaction
  with TransactionV3 {

  override val builder: TransactionParser = AnchorTransactionV3
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(builder.typeId, version),
      Longs.toByteArray(timestamp),
      sender.publicKey,
      Longs.toByteArray(fee),
      Deser.serializeArrays(anchors.map(_.arr)),
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

object AnchorTransactionV3 extends TransactionParserFor[AnchorTransactionV3] with AnchorTransactionParser {

  override val supportedVersions: Set[Byte] = Set(3)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val txEi = for {
        parsed  <- TransactionV3.parseBase(bytes, 0)
        (timestamp, sender, feeAmount, end) = parsed
        r <- Try(Deser.parseArraysPos(bytes.drop(end))).toEither.left.map(x => GenericError(x.toString))
        arrays  = r._1
        pos     = r._2
        sponsor <- TransactionV3.parseSponsor(bytes, end + pos)
        sponsorKeyLength: Short = sponsor.map(account => account.keyType.length).getOrElse(0)
        proofs  <- Proofs.fromBytes(bytes.drop(end + pos + 1 + sponsorKeyLength))
        tx      <- create(version, timestamp, sender, feeAmount, arrays.map(ByteStr(_)).toList, sponsor, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             timestamp: Long,
             sender: PublicKeyAccount,
             feeAmount: Long,
             data: List[ByteStr],
             sponsor: Option[PublicKeyAccount],
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    validate(version, data, feeAmount).right.map { _ =>
      AnchorTransactionV3(version, timestamp, sender, feeAmount, data, sponsor, proofs)
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
}
