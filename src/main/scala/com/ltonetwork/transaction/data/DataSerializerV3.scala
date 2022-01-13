package com.ltonetwork.transaction.data

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.serialization._
import com.ltonetwork.state.DataEntry
import com.ltonetwork.transaction.TransactionParser.{parseBase, parseFooter}
import com.ltonetwork.transaction.TransactionSerializer
import com.ltonetwork.transaction.data.DataTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object DataSerializerV3 extends TransactionSerializer.For[DataTransaction] {
  override def bodyBytes(tx: DataTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Shorts.toByteArray(data.size.toShort),
      data.flatMap(_.toBytes).toArray
    )
  }

  private def parseData(bytes: Array[Byte], pos: Int): (List[DataEntry[_]], Int) = {
    val entryCount = Shorts.fromByteArray(bytes.slice(pos, pos + Shorts.BYTES))

    if (entryCount > 0) {
      val parsed = List.iterate(DataEntry.parse(bytes, pos + Shorts.BYTES), entryCount) { case (_, p) => DataEntry.parse(bytes, p) }
      (parsed.map(_._1), parsed.last._2)
    } else (List.empty, pos + Shorts.BYTES)
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[DataTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val (entries, entriesEnd)             = parseData(bytes, buf.position)
      buf.position(entriesEnd)
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, entries, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
