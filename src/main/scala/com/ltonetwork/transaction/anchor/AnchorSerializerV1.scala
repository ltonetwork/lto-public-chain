package com.ltonetwork.transaction.anchor

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.{Proofs, TransactionSerializer}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.anchor.AnchorTransaction.create
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

object AnchorSerializerV1 extends TransactionSerializer.For[AnchorTransaction] {
  override def bodyBytes(tx: AnchorTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(AnchorTransaction.typeId, version),
      sender.publicKey,
      Deser.serializeArrays(anchors.map(_.arr)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[AnchorTransaction] =
    Try {
      val sender = PublicKeyAccount(bytes.take(KeyLength))

      (for {
        r <- Try(Deser.parseArraysPos(bytes.drop(KeyLength))).toEither.left.map(x => GenericError(x.toString))
        arrays    = r._1
        pos       = r._2
        timestamp = Longs.fromByteArray(bytes.drop(KeyLength + pos))
        fee       = Longs.fromByteArray(bytes.drop(KeyLength + pos + Longs.BYTES))

        proofs <- Proofs.fromBytes(bytes.drop(KeyLength + pos + Longs.BYTES + Longs.BYTES))
        tx     <- create(version, None, timestamp, sender, fee, arrays.map(ByteStr(_)).toList, None, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
