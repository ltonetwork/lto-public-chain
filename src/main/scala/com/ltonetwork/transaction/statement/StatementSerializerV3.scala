package com.ltonetwork.transaction.statement

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.statement.StatementTransaction.create
import com.ltonetwork.transaction.data.DataSerializerV3.parseData
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object StatementSerializerV3 extends TransactionSerializer.For[StatementTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: StatementTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(StatementTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Longs.toByteArray(statementType),
      Deser.serializeOption(recipient)(_.bytes.arr),
      Deser.serializeArray(subject.fold(Array.emptyByteArray)(_.arr)),
      Shorts.toByteArray(data.size.toShort),
      data.flatMap(_.toBytes).toArray
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[StatementTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val statementType                     = buf.getLong
      val recipient                         = buf.getOption(_.getAddress)
      val subject                           = Some(ByteStr(buf.getByteArrayWithLength)).noneIfEmpty
      val data                              = parseData(buf)
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, statementType, recipient, subject, data, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
