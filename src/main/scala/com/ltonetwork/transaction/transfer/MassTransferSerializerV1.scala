package com.ltonetwork.transaction.transfer

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.{Proofs, TransactionParser, TransactionSerializer, ValidationError}
import com.ltonetwork.transaction.ValidationError.Validation
import com.ltonetwork.transaction.transfer.MassTransferTransaction.{ParsedTransfer, Transfer, create}
import com.ltonetwork.utils.Base58
import monix.eval.Coeval
import play.api.libs.json.{Format, JsObject, JsValue, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object MassTransferSerializerV1 extends TransactionSerializer.For[MassTransferTransaction] {
  implicit val transferFormat: Format[Transfer] = Json.format

  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._

    val transferBytes = transfers
      .map { case ParsedTransfer(recipient, amount) => recipient.bytes.arr ++ Longs.toByteArray(amount) }
      .fold(Array())(_ ++ _)

    Bytes.concat(
      Array(MassTransferTransaction.typeId, version),
      sender.publicKey,
      Shorts.toByteArray(transfers.size.toShort),
      transferBytes,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee),
      Deser.serializeArray(attachment)
    )
  }

  private def parseTransfer(buf: ByteBuffer): ParsedTransfer = {
    val address = buf.getAddress
    val amount  = buf.getLong

    ParsedTransfer(address, amount)
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val sender        = buf.getPublicKey
    val transferCount = buf.getShort

    val transfers = (0 until transferCount).foldLeft(List.empty[ParsedTransfer]) {
      case (acc, _) => acc :+ parseTransfer(buf)
    }

    val timestamp  = buf.getLong
    val fee        = buf.getLong
    val attachment = buf.getByteArrayWithLength
    val proofs     = buf.getProofs

    create(version, None, timestamp, sender, fee, transfers, attachment, None, proofs)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
