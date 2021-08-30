package com.ltonetwork.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}
import com.ltonetwork.transaction.lease.LeaseTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object LeaseSerializerV3 extends TransactionSerializer.For[LeaseTransaction] {
  import TransactionParser._

  def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._
    Bytes.concat(
      Array(builder.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Longs.toByteArray(amount),
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val recipient                         = buf.getAddress
      val amount                            = buf.getLong
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, recipient, amount, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
