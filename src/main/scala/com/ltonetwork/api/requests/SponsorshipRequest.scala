package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.sponsorship.SponsorshipTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json.{Format, JsObject, Json}

case class SponsorshipRequest(version: Option[Byte] = None,
                              timestamp: Option[Long] = None,
                              fee: Long,
                              sender: Option[String] = None,
                              senderKeyType: Option[String] = None,
                              senderPublicKey: Option[String] = None,
                              sponsor: Option[String] = None,
                              sponsorKeyType: Option[String] = None,
                              sponsorPublicKey: Option[String] = None,
                              recipient: String,
                              signature: Option[ByteStr] = None,
                              proofs: Option[Proofs] = None)
    extends TxRequest.For[SponsorshipTransaction] {

  protected def sign(tx: SponsorshipTransaction, signer: PrivateKeyAccount): SponsorshipTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, SponsorshipTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      tx <- SponsorshipTransaction.create(
        version.getOrElse(SponsorshipTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        validRecipient,
        sponsor,
        proofs
      )
    } yield tx
}

object SponsorshipRequest {
  implicit val jsonFormat: Format[SponsorshipRequest] = Format(
    Json.reads[SponsorshipRequest],
    Json.writes[SponsorshipRequest].transform((json: JsObject) => Json.obj("type" -> SponsorshipTransaction.typeId.toInt) ++ json)
  )
}
