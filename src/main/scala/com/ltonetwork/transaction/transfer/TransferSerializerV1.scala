package com.ltonetwork.transaction.transfer

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.ltonetwork.account.{AddressOrAlias, PublicKeyAccount}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.{Proofs, TransactionSerializer, ValidationError}
import com.ltonetwork.transaction.ValidationError.Validation
import com.ltonetwork.transaction.transfer.TransferTransaction.TransactionT
import com.ltonetwork.utils.Base58
import monix.eval.Coeval
import play.api.libs.json.{Format, JsObject, Json}
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}

import scala.util.{Failure, Success, Try}

object TransferSerializerV1 extends TransactionSerializer.For[TransferTransaction] {
  implicit val transferFormat: Format[Transfer] = Json.format

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "recipient"  -> recipient.stringRepr,
      "amount"     -> amount,
      "attachment" -> Base58.encode(attachment)
    )
  )

  override def bodyBytes(tx: TransactionT): Coeval[Array[Byte]] = Coeval.evalOnce {
    import tx._

    Bytes.concat(
      Array(TransferTransaction.typeId),
      sender.publicKey,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(amount),
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Deser.serializeArray(attachment)
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =



  def parseBase(bytes: Array[Byte], start: Int) = {
    val sender    = PublicKeyAccount(bytes.slice(start, start + KeyLength))
    val s1        = start + KeyLength
    val timestamp = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
    val amount    = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
    val fee = Longs.fromByteArray(bytes.slice(s1 + 16, s1 + 24))
    for {
      recRes <- AddressOrAlias.fromBytes(bytes, s1 + 24)
      (recipient, recipientEnd) = recRes
      (attachment, end)         = Deser.parseArraySize(bytes, recipientEnd)
    } yield (sender, timestamp, amount, fee, recipient, attachment, end)

  }

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")

      (for {
        parsed <- TransferTransaction.parseBase(bytes, SignatureLength + 1)
        (sender, timestamp, amount, fee, recipient, attachment, _) = parsed
        tt <- TransferTransaction.create(sender, recipient, amount, timestamp, fee, attachment, signature)
      } yield tt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
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

}
