package com.ltonetwork.transaction.anchor

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.{Proofs, TransactionParser, TransactionSerializer}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

object AnchorSerializerV3 extends TransactionSerializer.For[AnchorTransaction] {
  override def bodyBytes(tx: AnchorTransaction): Coeval[Array[Byte]] = Coeval.evalOnce {
    import tx._

    Bytes.concat(
      Array(AnchorTransaction.typeId, version),
      Longs.toByteArray(timestamp),
      sender.publicKey,
      Longs.toByteArray(fee),
      Deser.serializeArrays(anchors.map(_.arr)),
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[AnchorTransaction] =
    Try {
      val txEi = for {
        parsed  <- TransactionParser.parseBase(bytes, 0)
        (timestamp, sender, fee, end) = parsed
        r <- Try(Deser.parseArraysPos(bytes.drop(end))).toEither.left.map(x => GenericError(x.toString))
        anchors = r._1
        pos     = r._2
        sponsor <- TransactionParser.parseSponsor(bytes, end + pos)
        sponsorKeyLength: Short = sponsor.map(account => account.keyType.length).getOrElse(0)
        proofs  <- Proofs.fromBytes(bytes.drop(end + pos + 1 + sponsorKeyLength))
        tx      <- AnchorTransaction.create(version, timestamp, sender, fee, anchors.map(ByteStr(_)).toList, sponsor, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  override def toJson(tx: AnchorTransaction): Coeval[JsObject] = Coeval.evalOnce {
    jsonBase(
      tx,
      Json.obj("anchors" -> Json.toJson(tx.anchors))
    )
  }
}
