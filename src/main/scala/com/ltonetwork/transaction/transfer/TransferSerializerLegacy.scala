package com.ltonetwork.transaction.transfer

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.TransactionSerializer

import java.nio.ByteBuffer

// Common methods for TransferSerializer v1 and v2
trait TransferSerializerLegacy extends TransactionSerializer.For[TransferTransaction] {
  def parseBase(buf: ByteBuffer): (PublicKeyAccount, Long, Long, Long, Address, Array[Byte]) = {
    val sender     = buf.getPublicKey
    val timestamp  = buf.getLong
    val amount     = buf.getLong
    val feeAmount  = buf.getLong
    val recipient  = buf.getAddress
    val attachment = buf.getByteArrayWithLength

    (sender, timestamp, amount, feeAmount, recipient, attachment)
  }
}
