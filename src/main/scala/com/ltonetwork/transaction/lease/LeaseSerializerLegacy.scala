package com.ltonetwork.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.transaction.{TransactionSerializer, ValidationError}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

// Common methods for LeaseSerializer v1 and v2
trait LeaseSerializerLegacy extends TransactionSerializer.For[LeaseTransaction] {
  def bytesBase(tx: TransactionT): Array[Byte] = {
    import tx._
    Bytes.concat(
      sender.publicKey,
      recipient.bytes.arr,
      Longs.toByteArray(amount),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }

  def parseBase(bytes: Array[Byte], start: Int): Either[ValidationError, (PublicKeyAccount, Address, Long, Long, Long, Int)] = {
    val sender = PublicKeyAccount(bytes.slice(start, start + KeyLength))
    for {
      recRes <- Address.fromBytes(bytes, start + KeyLength)
      (recipient, recipientEnd) = recRes
      amountStart               = recipientEnd
      amount                    = Longs.fromByteArray(bytes.slice(amountStart, amountStart + 8))
      fee                       = Longs.fromByteArray(bytes.slice(amountStart + 8, amountStart + 16))
      end                       = amountStart + 24
      timestamp                 = Longs.fromByteArray(bytes.slice(amountStart + 16, end))
    } yield (sender, recipient, amount, fee, timestamp, end)
  }

  override def toJson(tx: TransactionT): Coeval[JsObject] = Coeval.evalOnce {
    import tx._
    jsonBase(
      tx,
      Json.obj(
        "recipient"  -> recipient.stringRepr,
        "amount"     -> amount
      )
    )
  }
}
