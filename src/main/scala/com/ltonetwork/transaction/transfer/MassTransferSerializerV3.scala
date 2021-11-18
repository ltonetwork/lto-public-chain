package com.ltonetwork.transaction.transfer

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}
import com.ltonetwork.transaction.transfer.MassTransferTransaction.{ParsedTransfer, Transfer, create}
import play.api.libs.json.{Format, Json}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object MassTransferSerializerV3 extends TransactionSerializer.For[MassTransferTransaction] {
  import TransactionParser._

  implicit val transferFormat: Format[Transfer] = Json.format

  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._

    val transferBytes = transfers
      .map { case ParsedTransfer(recipient, amount) => recipient.bytes.arr ++ Longs.toByteArray(amount) }
      .fold(Array())(_ ++ _)

    Bytes.concat(
      Array(MassTransferTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Shorts.toByteArray(transfers.size.toShort),
      transferBytes,
      Deser.serializeArray(attachment)
    )
  }

  private def parseTransfer(buf: ByteBuffer): ParsedTransfer = {
    val address = buf.getAddress
    val amount  = buf.getLong

    ParsedTransfer(address, amount)
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)

      val transferCount = buf.getShort
      val transfers = (0 until transferCount).foldLeft(List.empty[ParsedTransfer]) {
        case (acc, _) => acc :+ parseTransfer(buf)
      }

      val attachment        = buf.getByteArrayWithLength
      val (sponsor, proofs) = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, transfers, attachment, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
