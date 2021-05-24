package com.ltonetwork.transaction.anchor

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.{Proofs, TransactionSerializer}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.anchor.AnchorTransaction.create
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

object AnchorSerializerV1 extends TransactionSerializer.For[AnchorTransaction] {
  override def bodyBytes(tx: AnchorTransaction): Coeval[Array[Byte]] = Coeval.evalOnce {
    import tx._

    Bytes.concat(
      Array(AnchorTransaction.typeId, version),
      sender.publicKey,
      Deser.serializeArrays(anchors.map(_.arr)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  override protected def parseBytes(version: Byte, bytes: Array[Byte]): Try[AnchorTransaction] =
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
        tx     <- create(version, timestamp, sender, feeAmount, arrays.map(ByteStr(_)).toList, None, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  override def json(tx: AnchorTransaction): Coeval[JsObject] = Coeval.evalOnce {
    jsonBase(
      tx,
      Json.obj("anchors" -> Json.toJson(tx.anchors))
    )
  }
}
