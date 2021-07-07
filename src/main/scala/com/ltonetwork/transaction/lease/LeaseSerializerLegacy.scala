package com.ltonetwork.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.TransactionSerializer
import scorex.crypto.signatures.Curve25519.KeyLength

import java.nio.ByteBuffer

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

  def parseBase(buf: ByteBuffer): (PublicKeyAccount, Address, Long, Long, Long) = {
    val sender    = buf.getPublicKey
    val recipient = buf.getAddress
    val amount    = buf.getLong
    val fee       = buf.getLong
    val timestamp = buf.getLong

    (sender, recipient, amount, fee, timestamp)
  }
}
