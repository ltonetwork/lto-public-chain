package com.ltonetwork.transaction.data

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.serialization._
import com.ltonetwork.state.DataEntry
import com.ltonetwork.transaction.data.DataTransaction.create
import com.ltonetwork.transaction.{Proofs, TransactionSerializer}
import scorex.crypto.signatures.Curve25519.KeyLength

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object DataSerializerV1 extends TransactionSerializer.For[DataTransaction] {
  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      Shorts.toByteArray(data.size.toShort),
      data.flatMap(_.toBytes).toArray,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  private def parseData(bytes: Array[Byte], pos: Int): (List[DataEntry[_]], Int) = {
    val entryCount = Shorts.fromByteArray(bytes.slice(pos, pos + Shorts.BYTES))

    if (entryCount > 0) {
      val parsed = List.iterate(DataEntry.parse(bytes, pos + Shorts.BYTES), entryCount) { case (_, p) => DataEntry.parse(bytes, p) }
      (parsed.map(_._1), parsed.last._2)
    } else (List.empty, pos + Shorts.BYTES)
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val sender = buf.getPublicKey

      val (entries, entriesEnd) = parseData(bytes, buf.position)
      buf.position(entriesEnd)

      val timestamp = buf.getLong
      val fee       = buf.getLong
      val proofs    = buf.getProofs

      create(version, None, timestamp, sender, fee, entries, None, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
