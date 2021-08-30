package com.ltonetwork.transaction.transfer

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.transfer.TransferTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object TransferSerializerV2 extends TransferSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(TransferTransaction.typeId, version),
      sender.publicKey,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(amount),
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Deser.serializeArray(attachment)
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (sender, timestamp, amount, fee, recipient, attachment) = parseBase(buf)
      val proofs                                                  = buf.getProofs

      create(version, None, timestamp, sender, fee, recipient, amount, attachment, None, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
