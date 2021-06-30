package com.ltonetwork.transaction.sponsorship

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.transaction.{Proofs, TransactionSerializer, ValidationError}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

abstract class SponsorshipSerializerV1[SponsorshipTransactionT <: SponsorshipTransactionBase]
    extends TransactionSerializer.For[SponsorshipTransactionT] {
  override def bodyBytes(tx: SponsorshipTransactionT): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      sender.publicKey,
      recipient.bytes.arr,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  protected def parseBase(bytes: Array[Byte]): Either[ValidationError, (Byte, Long, PublicKeyAccount, Long, Address, Proofs)] = {
    val chainId = bytes(0)
    val p0      = KeyLength
    val sender  = PublicKeyAccount(bytes.slice(1, p0 + 1))

    for {
      recipient <- Address.fromBytes(bytes.slice(p0 + 1, p0 + 1 + Address.AddressLength))
      recipientEnd = p0 + 1 + Address.AddressLength
      s1           = recipientEnd
      timestamp    = Longs.fromByteArray(bytes.drop(s1))
      fee          = Longs.fromByteArray(bytes.drop(s1 + 8))
      proofs <- Proofs.fromBytes(bytes.drop(s1 + 16))
    } yield (chainId, timestamp, sender, fee, recipient, proofs)
  }
}
