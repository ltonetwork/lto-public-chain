package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.Address
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.association.IssueAssociationTransaction.create
import com.ltonetwork.transaction.{Proofs, TransactionParser, TransactionSerializer}

import scala.util.{Failure, Success, Try}

class IssueAssociationSerializerV3 extends TransactionSerializer.For[IssueAssociationTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: IssueAssociationTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      sender.publicKey,
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Ints.toByteArray(assocType),
      Longs.toByteArray(expires.getOrElse(0)),
      hash.fold(Array(0: Byte))(a => Deser.serializeArray(a.arr))
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[IssueAssociationTransaction] =
    Try {
      (for {
        parsed <- parseBase(bytes)
        (chainId, timestamp, sender, fee, end) = parsed
        recipient <- Address.fromBytes(bytes.slice(end, end + Address.AddressLength))
        recipientEnd    = end + Address.AddressLength
        assocType       = Ints.fromByteArray(bytes.slice(recipientEnd, recipientEnd + Ints.BYTES))
        expires         = Longs.fromByteArray(bytes.slice(recipientEnd + Ints.BYTES, recipientEnd + Ints.BYTES + Longs.BYTES))
        expiresOpt      = Some(expires).noneIf(0)
        (hashBytes, s1) = Deser.parseArraySize(bytes, recipientEnd + Ints.BYTES + Longs.BYTES)
        hashOpt         = Some(ByteStr(hashBytes)).noneIfEmpty
        sponsor <- parseSponsor(bytes, s1)
        s2              = s1 + sponsor.fold(0)(account => account.keyType.length)
        proofs  <- Proofs.fromBytes(bytes.drop(s2))
        tx      <- create(version, Some(chainId), timestamp, sender, fee, recipient, assocType, expiresOpt, hashOpt, sponsor, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
