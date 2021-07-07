package com.ltonetwork.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.TransactionSerializer

import java.nio.ByteBuffer

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

  def parseBase(buf: ByteBuffer): (PublicKeyAccount, Long, Long, ByteStr) = {
    val sender    = buf.getPublicKey
    val fee       = buf.getLong
    val timestamp = buf.getLong
    val leaseId   = ByteStr(buf.getByteArray(16))
    (sender, fee, timestamp, leaseId)
  }
}
