package com.ltonetwork.transaction.sponsorship

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.{Proofs, TransactionParser, TransactionSerializer, ValidationError}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

trait SponsorshipSerializerV3[SponsorshipTransactionT <: SponsorshipTransactionBase]
    extends TransactionSerializer.For[SponsorshipTransactionT] {

  import TransactionParser._

  type CreateCtor = (Byte, Option[Byte], Long, PublicKeyAccount, Long, Address, Option[PublicKeyAccount], Proofs) => Either[ValidationError, SponsorshipTransactionT]
  protected val createTx: CreateCtor

  def bodyBytes(tx: SponsorshipTransactionT): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(builder.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      recipient.bytes.arr,
    )
  }

  def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val (chainId, timestamp, sender, fee) = parseBase(buf)
    val recipient = buf.getAddress
    val (sponsor, proofs) = parseFooter(buf)

    createTx(version, Some(chainId), timestamp, sender, fee, recipient, sponsor, proofs)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
