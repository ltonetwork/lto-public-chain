package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.association.IssueAssociationTransaction.create
import com.ltonetwork.transaction.data.DataSerializerV3.parseData
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object IssueAssociationSerializerV4 extends TransactionSerializer.For[IssueAssociationTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: IssueAssociationTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Ints.toByteArray(assocType),
      Longs.toByteArray(expires.getOrElse(0)),
      Deser.serializeArray(hash.fold(Array.emptyByteArray)(_.arr))
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[IssueAssociationTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val recipient                         = buf.getAddress
      val assocType                         = buf.getInt
      val expires                           = Some(buf.getLong).noneIf(0)
      val hash                              = Some(buf.getByteArrayWithLength).map(ByteStr(_)).noneIfEmpty
      val data                              = parseData(buf)
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, recipient, assocType, expires, hash, data, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
