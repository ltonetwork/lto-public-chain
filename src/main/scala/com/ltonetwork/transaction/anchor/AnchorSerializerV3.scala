package com.ltonetwork.transaction.anchor

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.{Proofs, TransactionParser, TransactionSerializer}

import scala.util.{Failure, Success, Try}

object AnchorSerializerV3 extends TransactionSerializer.For[AnchorTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: AnchorTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(AnchorTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Array(sender.keyType.id),
      sender.publicKey,
      Longs.toByteArray(fee),
      Deser.serializeArrays(anchors.map(_.arr)),
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[AnchorTransaction] =
    Try {
      (for {
        parsed <- parseBase(bytes)
        (chainId, timestamp, sender, fee, end) = parsed
        r <- Try(Deser.parseArraysPos(bytes.drop(end))).toEither.left.map(x => GenericError(x.toString))
        anchors = r._1
        pos     = r._2
        sponsor <- parseSponsor(bytes, end + pos)
        sponsorKeyLength: Short = sponsor.fold(0: Short)(account => account.keyType.length)
        proofs <- Proofs.fromBytes(bytes.drop(end + pos + 1 + sponsorKeyLength))
        tx     <- AnchorTransaction.create(version, Some(chainId), timestamp, sender, fee, anchors.map(ByteStr(_)).toList, sponsor, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
