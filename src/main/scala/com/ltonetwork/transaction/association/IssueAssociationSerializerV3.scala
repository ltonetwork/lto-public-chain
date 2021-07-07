package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.Address
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.association.IssueAssociationTransaction.create
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object IssueAssociationSerializerV3 extends TransactionSerializer.For[IssueAssociationTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: IssueAssociationTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      sender.publicKey,
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Ints.toByteArray(assocType),
      Longs.toByteArray(expires.getOrElse(0)),
      hash.fold(Array(0: Byte))(a => Deser.serializeArray(a.arr))
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[IssueAssociationTransaction] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val (chainId, timestamp, sender, fee) = parseBase(buf)
    val recipient = buf.getAddress
    val assocType = buf.getInt
    val expires   = Some(buf.getLong).noneIf(0)
    val hash      = Some(buf.getByteArrayWithLength).map(ByteStr(_)).noneIfEmpty
    val sponsor   = parseSponsor(buf)
    val proofs    = buf.getProofs

    create(version, Some(chainId), timestamp, sender, fee, recipient, assocType, expires, hash, sponsor, proofs)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
