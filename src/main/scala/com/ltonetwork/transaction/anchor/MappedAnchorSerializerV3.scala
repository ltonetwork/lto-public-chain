package com.ltonetwork.transaction.anchor

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.anchor.MappedAnchorTransaction.create
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object MappedAnchorSerializerV3 extends TransactionSerializer.For[MappedAnchorTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: MappedAnchorTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(MappedAnchorTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Deser.serializeMap(anchors) {
        (k: ByteStr, v: ByteStr) => Bytes.concat(Deser.serializeArray(k), Deser.serializeArray(v))
      },
    )
  }

  private def parseAnchors(buf: ByteBuffer) = buf
    .getAll { buf => (buf.getByteArrayWithLength, buf.getByteArrayWithLength) }
    .map { case (k, v) => (ByteStr(k), ByteStr(v)) }
    .toMap

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[MappedAnchorTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val anchors                           = parseAnchors(buf)
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, anchors, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
