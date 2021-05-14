package com.ltonetwork.transaction

import com.google.common.primitives.Longs
import com.ltonetwork.account.KeyTypes._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.transaction.ValidationError.InvalidPublicKey
import com.ltonetwork.utils.Base58
import play.api.libs.json.{JsObject, Json}

trait TransactionV3 extends ProvenTransaction with Sponsored {
  protected def sponsorField: JsObject = {
    if (sponsor.keyType != NO_KEY)
      Json.obj(
        "sponsor"          -> sponsor.address,
        "sponsorKeyType"   -> sponsor.keyType.id,
        "sponsorPublicKey" -> Base58.encode(sponsor.publicKey)
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
      accountKeyType => PublicKeyAccount(accountKeyType, bytes.slice(start + 1, start + 1 + accountKeyType.length))
    }
  }

  def parseBase(bytes: Array[Byte], start: Int): Either[ValidationError, (Long, PublicKeyAccount, PublicKeyAccount, Long)] = {
    val timestamp = Longs.fromByteArray(bytes.slice(start, start + longLength))

    val senderMaybe = parsePublicKeyAccount(bytes, start + longLength)
    if (senderMaybe.isEmpty) {
      return Left(InvalidPublicKey("Invalid sender key type"))
    }
    val sender = senderMaybe.get

    val sponsorMaybe = parsePublicKeyAccount(bytes, start + 9 + sender.keyType.length)
    if (sponsorMaybe.isEmpty) {
      return Left(InvalidPublicKey("Invalid sponsor key type"))
    }
    val sponsor = sponsorMaybe.get

    val s1 = start + longLength + 2 + sender.keyType.length + sponsor.keyType.length
    val feeAmount = Longs.fromByteArray(bytes.slice(s1, s1 + longLength))

    Right(timestamp, sender, sponsor, feeAmount)
  }

}
