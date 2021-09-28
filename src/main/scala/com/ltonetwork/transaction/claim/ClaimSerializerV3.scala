package com.ltonetwork.transaction.claim

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.claim.ClaimTransaction.create
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object ClaimSerializerV3 extends TransactionSerializer.For[ClaimTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: ClaimTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(ClaimTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Deser.serializeArrays(anchors.map(_.arr)),
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[ClaimTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val anchors                           = buf.getArrays.map(ByteStr(_)).toList
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, anchors, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
