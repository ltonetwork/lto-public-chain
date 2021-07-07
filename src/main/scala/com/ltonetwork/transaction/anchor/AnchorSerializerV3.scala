package com.ltonetwork.transaction.anchor

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.anchor.AnchorTransaction.create
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object AnchorSerializerV3 extends TransactionSerializer.For[AnchorTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: AnchorTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(AnchorTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Array(sender.keyType.id),
      sender.publicKey,
      Longs.toByteArray(fee),
      Deser.serializeArrays(anchors.map(_.arr)),
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[AnchorTransaction] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val (chainId, timestamp, sender, fee) = parseBase(buf)
    val anchors = buf.getArrays.map(ByteStr(_)).toList
    val sponsor = parseSponsor(buf)
    val proofs  = buf.getProofs

    create(version, Some(chainId), timestamp, sender, fee, anchors, sponsor, proofs)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
