package com.ltonetwork.transaction.register

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.register.RegisterTransaction.create
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object RegisterSerializerV3 extends TransactionSerializer.For[RegisterTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: RegisterTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(RegisterTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Shorts.toByteArray(accounts.length.toShort),
      Deser.serializeArraysWithoutLength(accounts.map(k => Deser.serializeAccount(k))),
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[RegisterTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val keys                              = buf.getAll(b => b.getAccount)
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, keys.toList, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
