package com.ltonetwork.transaction.data

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.DataEntry
import com.ltonetwork.transaction.data.DataTransaction.create
import com.ltonetwork.transaction.{Proofs, TransactionSerializer}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength
import scala.util.{Failure, Success, Try}

object DataSerializerV1 extends TransactionSerializer.For[DataTransaction] {
  override def bodyBytes(tx: TransactionT): Coeval[Array[Byte]] = Coeval.evalOnce {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      Shorts.toByteArray(data.size.toShort),
      data.flatMap(_.toBytes).toArray,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  override protected def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val p0     = KeyLength
      val sender = PublicKeyAccount(bytes.slice(0, p0))

      val entryCount = Shorts.fromByteArray(bytes.drop(p0))
      val (entries, p1) =
        if (entryCount > 0) {
          val parsed = List.iterate(DataEntry.parse(bytes, p0 + 2), entryCount) { case (e, p) => DataEntry.parse(bytes, p) }
          (parsed.map(_._1), parsed.last._2)
        } else (List.empty, p0 + 2)

      val timestamp = Longs.fromByteArray(bytes.drop(p1))
      val fee = Longs.fromByteArray(bytes.drop(p1 + 8))
      (for {
        proofs <- Proofs.fromBytes(bytes.drop(p1 + 16))
        tx     <- create(version, None, timestamp, sender, fee, entries, None, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  override def toJson(tx: TransactionT): Coeval[JsObject] = Coeval.evalOnce {
    jsonBase(
      tx,
      Json.obj("data" -> Json.toJson(tx.data))
    )
  }
}
