package com.ltonetwork.transaction.transfer

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.account.{AddressOrAlias, PublicKeyAccount}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.transaction.{Proofs, TransactionSerializer, ValidationError}
import com.ltonetwork.transaction.ValidationError.Validation
import com.ltonetwork.transaction.transfer.MassTransferTransaction.{ParsedTransfer, Transfer}
import com.ltonetwork.utils.Base58
import monix.eval.Coeval
import play.api.libs.json.{Format, JsObject, JsValue, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

object MassTransferSerializerV1 extends TransactionSerializer.For[MassTransferTransaction] {
  implicit val transferFormat: Format[Transfer] = Json.format

  override def bodyBytes(tx: TransactionT): Coeval[Array[Byte]] = Coeval.evalOnce {
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

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val sender        = PublicKeyAccount(bytes.slice(0, KeyLength))
      val s0            = KeyLength
      val transferCount = Shorts.fromByteArray(bytes.slice(s0, s0 + 2))

      def readTransfer(offset: Int): (Validation[ParsedTransfer], Int) = {
        AddressOrAlias.fromBytes(bytes, offset) match {
          case Right((addr, ofs)) =>
            val amount = Longs.fromByteArray(bytes.slice(ofs, ofs + 8))
            (Right[ValidationError, ParsedTransfer](ParsedTransfer(addr, amount)), ofs + 8)
          case Left(e) => (Left(e), offset)
        }
      }

      val transfersList: List[(Validation[ParsedTransfer], Int)] =
        List.iterate(readTransfer(s0 + 2), transferCount) { case (_, offset) => readTransfer(offset) }

      val s1 = transfersList.lastOption.map(_._2).getOrElse(s0 + 2)
      val tx: Validation[MassTransferTransaction] = for {
        transfers <- transfersList.map { case (ei, _) => ei }.sequence
        timestamp               = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
        fee               = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
        (attachment, attachEnd) = Deser.parseArraySize(bytes, s1 + 16)
        proofs <- Proofs.fromBytes(bytes.drop(attachEnd))
        mtt    <- MassTransferTransaction.create(version, sender, transfers, timestamp, fee, attachment, proofs)
      } yield mtt
      tx.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  override def toJson(tx: MassTransferTransaction): Coeval[JsObject] = Coeval.evalOnce {
    jsonBase(
      tx,
      Json.obj(
        "attachment" -> Base58.encode(tx.attachment),
        "transferCount" -> tx.transfers.size,
        "totalAmount" -> tx.transfers.map(_.amount).sum,
        "transfers" -> toJson(tx.transfers)
      )
    )
  }

  private def toJson(transfers: List[ParsedTransfer]): JsValue =
    Json.toJson(transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) })
}
