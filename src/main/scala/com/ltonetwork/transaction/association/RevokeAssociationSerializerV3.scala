package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.Address
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.association.RevokeAssociationTransaction.create
import com.ltonetwork.transaction.{Proofs, TransactionParser, TransactionSerializer}

import scala.util.{Failure, Success, Try}

class RevokeAssociationSerializerV3 extends TransactionSerializer.For[RevokeAssociationTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: RevokeAssociationTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      sender.publicKey,
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Ints.toByteArray(assocType),
      hash.fold(Array(0: Byte))(a => Deser.serializeArray(a.arr))
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[RevokeAssociationTransaction] =
    Try {
      (for {
        parsed <- parseBase(bytes)
        (chainId, timestamp, sender, fee, end) = parsed
        recipient <- Address.fromBytes(bytes.slice(end, end + Address.AddressLength))
        recipientEnd    = end + Address.AddressLength
        assocType       = Ints.fromByteArray(bytes.slice(recipientEnd, recipientEnd + Ints.BYTES))
        (hashBytes, s1) = Deser.parseArraySize(bytes, recipientEnd + Ints.BYTES + Longs.BYTES)
        hashOpt         = Some(ByteStr(hashBytes)).noneIfEmpty
        sponsor <- parseSponsor(bytes, s1)
        s2              = s1 + sponsor.fold(0)(account => account.keyType.length)
        proofs  <- Proofs.fromBytes(bytes.drop(s2))
        tx      <- create(version, Some(chainId), timestamp, sender, fee, recipient, assocType, hashOpt, sponsor, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}