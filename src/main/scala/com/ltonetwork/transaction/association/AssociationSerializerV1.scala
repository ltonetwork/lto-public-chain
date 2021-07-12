package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.transaction.{Proofs, TransactionSerializer, ValidationError}

import java.nio.ByteBuffer
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
                         proofs: Proofs): Either[ValidationError, AssociationTransactionT]

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
    val buf = ByteBuffer.wrap(bytes)

    val chainId   = buf.getByte
    val sender    = buf.getPublicKey
    val recipient = buf.getAddress
    val assocType = buf.getInt
    val hashOpt   = buf.getOptionalByteArray.map(ByteStr(_))
    val timestamp = buf.getLong
    val fee       = buf.getLong
    val proofs    = buf.getProofs

    createTx(version, chainId, timestamp, sender, fee, recipient, assocType, hashOpt, proofs)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
