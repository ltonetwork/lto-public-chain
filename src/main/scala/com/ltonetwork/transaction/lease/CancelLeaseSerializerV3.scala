package com.ltonetwork.transaction.lease

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}
import com.ltonetwork.transaction.lease.CancelLeaseTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object CancelLeaseSerializerV3 extends TransactionSerializer.For[CancelLeaseTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._
    require(leaseId.arr.length == 32)

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      leaseId.arr,
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val (chainId, timestamp, sender, fee) = parseBase(buf)
    val leaseId = ByteStr(buf.getByteArray(32))
    val sponsor = parseSponsor(buf)
    val proofs  = buf.getProofs

    create(version, Some(chainId), timestamp, sender, fee, leaseId, sponsor, proofs)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
