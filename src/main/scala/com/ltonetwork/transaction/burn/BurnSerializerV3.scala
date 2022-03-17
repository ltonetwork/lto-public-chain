package com.ltonetwork.transaction.burn

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.burn.BurnTransaction.create
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object BurnSerializerV3 extends TransactionSerializer.For[BurnTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: BurnTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(BurnTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Shorts.toByteArray(accounts.length.toShort),
      Deser.serializeArraysWithoutLength(accounts.map(k => Deser.serializeAccount(k))),
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[BurnTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val keys                              = buf.getAll(b => b.getAccount)
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, keys.toList, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
