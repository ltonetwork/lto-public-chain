package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.{Proofs, TransactionSerializer, ValidationError}
import scorex.crypto.signatures.Curve25519.KeyLength

abstract class AssociationSerializerV1[AssociationTransactionT <: AssociationTransaction] extends TransactionSerializer.For[AssociationTransactionT] {
  override def bodyBytes(tx: AssociationTransactionT): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      sender.publicKey,
      recipient.bytes.arr,
      Ints.toByteArray(assocType),
      hash.fold(Array(0: Byte))(a => (1: Byte) +: Deser.serializeArray(a.arr)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  def parse(version: Byte,
            bytes: Array[Byte]): Either[ValidationError, (Byte, Long, PublicKeyAccount, Long, Address, Int, Option[ByteStr], Proofs)] = {
    val p0     = KeyLength
    val sender = PublicKeyAccount(bytes.slice(1, p0 + 1))

    for {
      recipient <- Address.fromBytes(bytes.slice(p0 + 1, p0 + 1 + Address.AddressLength))
      recipientEnd  = p0 + 1 + Address.AddressLength
      assocType     = Ints.fromByteArray(bytes.slice(recipientEnd, recipientEnd + 4))
      (hashOpt, s1) = Deser.parseOption(bytes, recipientEnd + 4)(ByteStr(_))
      timestamp     = Longs.fromByteArray(bytes.drop(s1))
      fee           = Longs.fromByteArray(bytes.drop(s1 + 8))
      proofs <- Proofs.fromBytes(bytes.drop(s1 + 16))
      result = (version, timestamp, sender, fee, recipient, assocType, hashOpt, proofs)
    } yield result
  }
}
