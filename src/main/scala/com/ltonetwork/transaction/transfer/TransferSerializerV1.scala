package com.ltonetwork.transaction.transfer

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.Proofs
import com.ltonetwork.transaction.transfer.TransferTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object TransferSerializerV1 extends TransferSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(TransferTransaction.typeId),
      sender.publicKey,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(amount),
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Deser.serializeArray(attachment)
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val signature = buf.getSignature

    val txTypeId  = buf.getByte
    require(txTypeId == TransferTransaction.typeId, s"Signed tx type ($txTypeId) doesn't match")

    val (sender, timestamp, amount, fee, recipient, attachment) = parseBase(buf)

    create(version, None, timestamp, sender, fee, recipient, amount, attachment, None, Proofs.fromSignature(signature))
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
