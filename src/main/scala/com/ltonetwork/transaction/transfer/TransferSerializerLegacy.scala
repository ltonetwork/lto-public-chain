package com.ltonetwork.transaction.transfer

import com.google.common.primitives.Longs
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.transaction.{TransactionSerializer, ValidationError}
import com.ltonetwork.utils.Base58
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

// Common methods for TransferSerializer v1 and v2
trait TransferSerializerLegacy extends TransactionSerializer.For[TransferTransaction] {
  def parseBase(bytes: Array[Byte], start: Int): Either[ValidationError, (PublicKeyAccount, Long, Long, Long, Address, Array[Byte], Int)] = {
    val sender = PublicKeyAccount(bytes.slice(start, start + KeyLength))

    val s1        = start + KeyLength
    val timestamp = Longs.fromByteArray(bytes.slice(s1, s1 + Longs.BYTES))
    val amount    = Longs.fromByteArray(bytes.slice(s1 + Longs.BYTES, s1 + 2 * Longs.BYTES))
    val feeAmount = Longs.fromByteArray(bytes.slice(s1 + 2 * Longs.BYTES, s1 + 3 * Longs.BYTES))

    for {
      recRes <- Address.fromBytes(bytes, s1 + 3 * Longs.BYTES)
      (recipient, recipientEnd) = recRes
      (attachment, end)         = Deser.parseArraySize(bytes, recipientEnd)
    } yield (sender, timestamp, amount, feeAmount, recipient, attachment, end)
  }
}
