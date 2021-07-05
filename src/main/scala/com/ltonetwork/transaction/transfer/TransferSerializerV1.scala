package com.ltonetwork.transaction.transfer

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.Proofs
import scorex.crypto.signatures.Curve25519.SignatureLength

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

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.take(SignatureLength))

      val txTypeId = bytes(SignatureLength)
      require(txTypeId == TransferTransaction.typeId, s"Signed tx type ($txTypeId) doesn't match")

      (for {
        parsed <- parseBase(bytes, SignatureLength + 1)
        (sender, timestamp, amount, fee, recipient, attachment, _) = parsed
        tx <- TransferTransaction.create(version, None, timestamp, sender, fee, recipient, amount, attachment, None, Proofs.fromSignature(signature))
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
