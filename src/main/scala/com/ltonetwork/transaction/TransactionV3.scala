package com.ltonetwork.transaction

import com.google.common.primitives.Longs
import com.ltonetwork.account.KeyTypes._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.transaction.ValidationError.InvalidPublicKey
import com.ltonetwork.utils.Base58
import play.api.libs.json.{JsObject, Json}

trait TransactionV3 extends ProvenTransaction with Sponsored with FastHashId {
  private def sponsorField: JsObject = {
    if (sponsor.isDefined)
      Json.obj(
        "sponsor"          -> sponsor.get.address,
        "sponsorKeyType"   -> sponsor.get.keyType.id,
        "sponsorPublicKey" -> Base58.encode(sponsor.get.publicKey)
      )
    else Json.obj()
  }

  override protected def jsonBase(): JsObject =
    Json.obj(
      "type"            -> builder.typeId,
      "id"              -> id().base58,
      "sender"          -> sender.address,
      "senderKeyType"   -> sender.keyType.id,
      "senderPublicKey" -> Base58.encode(sender.publicKey),
      "fee"             -> fee,
      "timestamp"       -> timestamp
    ) ++ sponsorField ++ Json.obj(proofField)
}

object TransactionV3 {
  private val longLength = 8

  private def parsePublicKeyAccount(bytes: Array[Byte], start: Int): Option[PublicKeyAccount] = {
    val keyTypeId = bytes(start)

    keyType(keyTypeId) map {
      kt => PublicKeyAccount(kt, bytes.slice(start + 1, start + 1 + kt.length))
    }
  }

  def parseSponsor(bytes: Array[Byte], start: Int): Either[ValidationError, Option[PublicKeyAccount]] = {
    val keyTypeId = bytes(start)

    if (keyTypeId == 0)
      Right(None)
    else
      parsePublicKeyAccount(bytes, start)
        .toRight(InvalidPublicKey("Invalid sender key type"))
        .right.map(sponsor => Some(sponsor))
  }
  
  def parseBase(bytes: Array[Byte], start: Int): Either[ValidationError, (Long, PublicKeyAccount, Long, Int)] = {
    val timestamp = Longs.fromByteArray(bytes.slice(start, start + longLength))

    val senderMaybe = parsePublicKeyAccount(bytes, start + longLength)

    if (senderMaybe.isEmpty)
      Left(InvalidPublicKey("Invalid sender key type"))
    else {
      val sender = senderMaybe.get

      val s1 = start + longLength + 2 + sender.keyType.length
      val feeAmount = Longs.fromByteArray(bytes.slice(s1, s1 + longLength))

      Right(timestamp, sender, feeAmount, s1 + longLength)
    }
  }
}
