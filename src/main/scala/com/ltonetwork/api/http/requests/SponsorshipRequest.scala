package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.sponsorship.SponsorshipTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json}

case class SponsorshipRequest(version: Option[Byte] = None,
                              timestamp: Option[Long] = None,
                              senderKeyType: Option[String] = None,
                              senderPublicKey: Option[String] = None,
                              fee: Long,
                              recipient: String,
                              sponsorKeyType: Option[String] = None,
                              sponsorPublicKey: Option[String] = None,
                              signature: Option[ByteStr] = None,
                              proofs: Option[Proofs] = None
    ) extends TxRequest.For[SponsorshipTransaction] {

  protected def sign(tx: SponsorshipTransaction, signer: PrivateKeyAccount): SponsorshipTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, SponsorshipTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      tx <- SponsorshipTransaction.create(
        version.getOrElse(SponsorshipTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        sponsor,
        validProofs
      )
    } yield tx
}

object SponsorshipRequest {
  implicit val jsonFormat: Format[SponsorshipRequest] = Format(
    Json.reads[SponsorshipRequest],
    Json.writes[SponsorshipRequest].transform((json: JsObject) => Json.obj("type" -> SponsorshipTransaction.typeId.toInt) ++ json)
  )
}
