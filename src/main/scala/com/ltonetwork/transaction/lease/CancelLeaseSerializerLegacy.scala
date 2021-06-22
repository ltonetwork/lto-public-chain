package com.ltonetwork.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.crypto
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.TransactionSerializer
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

// Common methods for CancelLeaseSerializer v1 and v2
trait CancelLeaseSerializerLegacy extends TransactionSerializer.For[CancelLeaseTransaction] {
  def bytesBase(tx: TransactionT): Array[Byte] = {
    import tx._
    Bytes.concat(
      sender.publicKey,
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp),
      leaseId.arr
    )
  }

  def parseBase(bytes: Array[Byte], start: Int): (PublicKeyAccount, Long, Long, ByteStr, Int) = {
    val sender    = PublicKeyAccount(bytes.slice(start, start + KeyLength))
    val fee       = Longs.fromByteArray(bytes.slice(start + KeyLength, start + KeyLength + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(start + KeyLength + 8, start + KeyLength + 16))
    val end       = start + KeyLength + 16 + crypto.DigestSize
    val leaseId   = ByteStr(bytes.slice(start + KeyLength + 16, end))
    (sender, fee, timestamp, leaseId, end)
  }

  override def toJson(tx: TransactionT): JsObject = {
    import tx._
    jsonBase(
      tx,
      Json.obj(
        "chainId"   -> chainId,
        "version"   -> version,
        "fee"       -> fee,
        "timestamp" -> timestamp,
        "leaseId"   -> leaseId.base58
      )
    )
  }
}
