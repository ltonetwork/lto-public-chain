package com.ltonetwork.transaction.claim

import com.google.common.primitives.{Bytes, Ints, Longs}
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

    val a = Bytes.concat(
      Array(ClaimTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Ints.toByteArray(claimType),
      Deser.serializeOption(recipient)(_.bytes.arr),
      Deser.serializeArray(subject.fold(Array.emptyByteArray)(_.arr)),
      Longs.toByteArray(amount),
      Deser.serializeArray(hash.fold(Array.emptyByteArray)(_.arr)),
      Deser.serializeList(related)(_.arr),
    )
    a
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[ClaimTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val claimType                         = buf.getInt
      val recipient                         = buf.getOption(_.getAddress)
      val subject                           = Some(ByteStr(buf.getByteArrayWithLength)).noneIfEmpty
      val amount                            = buf.getLong
      val hash                              = Some(ByteStr(buf.getByteArrayWithLength)).noneIfEmpty
      val related                           = buf.getList(_.getByteArray(32)).map(ByteStr(_))
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, claimType, recipient, subject, amount, hash, related, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
