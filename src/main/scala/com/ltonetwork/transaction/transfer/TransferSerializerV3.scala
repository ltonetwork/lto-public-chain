package com.ltonetwork.transaction.transfer

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}
import com.ltonetwork.transaction.transfer.TransferTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object TransferSerializerV3 extends TransactionSerializer.For[TransferTransaction] {
  import TransactionParser._

  def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(TransferTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Longs.toByteArray(amount),
      Deser.serializeArray(attachment)
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val (chainId, timestamp, sender, fee) = parseBase(buf)
    val recipient  = buf.getAddress
    val amount     = buf.getLong
    val attachment = buf.getByteArrayWithLength
    val (sponsor, proofs) = parseFooter(buf)

    create(version, Some(chainId), timestamp, sender, fee, recipient, amount, attachment, sponsor, proofs)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
