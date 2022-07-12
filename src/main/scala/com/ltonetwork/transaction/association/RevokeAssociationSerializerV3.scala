package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.association.RevokeAssociationTransaction.create
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object RevokeAssociationSerializerV3 extends TransactionSerializer.For[RevokeAssociationTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: RevokeAssociationTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Ints.toByteArray(assocType.toInt),
      recipient.bytes.arr,
      Deser.serializeArray(subject.fold(Array.emptyByteArray)(_.arr))
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[RevokeAssociationTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val recipient                         = buf.getAddress
      val assocType                         = buf.getInt
      val subject                           = Some(buf.getByteArrayWithLength).map(ByteStr(_)).noneIfEmpty
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, assocType, recipient, subject, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
