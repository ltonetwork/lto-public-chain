package com.ltonetwork.transaction.anchor

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.TransactionSerializer
import com.ltonetwork.transaction.anchor.AnchorTransaction.create

import java.nio.ByteBuffer
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

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[AnchorTransaction] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val sender    = buf.getPublicKey
    val anchors   = buf.getArrays.map(ByteStr(_)).toList
    val timestamp = buf.getLong
    val fee       = buf.getLong
    val proofs    = buf.getProofs

    create(version, None, timestamp, sender, fee, anchors, None, proofs)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
