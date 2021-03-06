package com.ltonetwork.transaction.transfer

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.transaction.Proofs

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

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      (for {
        parsed <- parseBase(bytes, 0)
        (sender, timestamp, amount, fee, recipient, attachment, end) = parsed
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tx     <- TransferTransaction.create(version, None, timestamp, sender, fee, recipient, amount, attachment, None, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
