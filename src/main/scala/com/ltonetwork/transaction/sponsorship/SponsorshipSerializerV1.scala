package com.ltonetwork.transaction.sponsorship

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.{Proofs, TransactionSerializer, ValidationError}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

trait SponsorshipSerializerV1[SponsorshipTransactionT <: SponsorshipTransactionBase] extends TransactionSerializer.For[SponsorshipTransactionT] {

  type CreateCtor =
    (Byte, Option[Byte], Long, PublicKeyAccount, Long, Address, Option[PublicKeyAccount], Proofs) => Either[ValidationError, SponsorshipTransactionT]
  protected val createTx: CreateCtor

  def bodyBytes(tx: SponsorshipTransactionT): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      sender.publicKey,
      recipient.bytes.arr,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val chainId   = buf.getByte
      val sender    = buf.getPublicKey
      val recipient = buf.getAddress
      val timestamp = buf.getLong
      val fee       = buf.getLong
      val proofs    = buf.getProofs

      createTx
        .apply(version, Some(chainId), timestamp, sender, fee, recipient, None, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
