package com.ltonetwork.transaction.claim

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.claim.ClaimTransaction.create
import com.ltonetwork.transaction.data.DataSerializerV3.parseData
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
      Longs.toByteArray(claimType),
      Deser.serializeOption(recipient)(_.bytes.arr),
      Deser.serializeArray(subject.fold(Array.emptyByteArray)(_.arr)),
      Shorts.toByteArray(data.size.toShort),
      data.flatMap(_.toBytes).toArray
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[ClaimTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val claimType                         = buf.getLong
      val recipient                         = buf.getOption(_.getAddress)
      val subject                           = Some(ByteStr(buf.getByteArrayWithLength)).noneIfEmpty
      val data                              = parseData(buf)
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, claimType, recipient, subject, data, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
