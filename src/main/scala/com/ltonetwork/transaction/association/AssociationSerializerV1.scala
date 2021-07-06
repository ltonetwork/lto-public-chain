package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.Validation
import com.ltonetwork.transaction.{Proofs, TransactionSerializer}
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

trait AssociationSerializerV1[AssociationTransactionT <: AssociationTransaction] extends TransactionSerializer.For[AssociationTransactionT] {
  protected def createTx(version: Byte,
                         chainId: Byte,
                         timestamp: Long,
                         sender: PublicKeyAccount,
                         fee: Long,
                         recipient: Address,
                         assocType: Int,
                         hash: Option[ByteStr],
                         proofs: Proofs): Validation[AssociationTransactionT]

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

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[AssociationTransactionT] = Try {
    val chainId = bytes(0)
    val p0      = KeyLength
    val sender  = PublicKeyAccount(bytes.slice(1, p0 + 1))

    (for {
      recipient <- Address.fromBytes(bytes.slice(p0 + 1, p0 + 1 + Address.AddressLength))
      recipientEnd  = p0 + 1 + Address.AddressLength
      assocType     = Ints.fromByteArray(bytes.slice(recipientEnd, recipientEnd + 4))
      (hashOpt, s1) = Deser.parseOption(bytes, recipientEnd + 4)(ByteStr(_))
      timestamp     = Longs.fromByteArray(bytes.drop(s1))
      fee           = Longs.fromByteArray(bytes.drop(s1 + 8))
      proofs <- Proofs.fromBytes(bytes.drop(s1 + 16))
      tx     <- createTx(version, chainId, timestamp, sender, fee, recipient, assocType, hashOpt, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
