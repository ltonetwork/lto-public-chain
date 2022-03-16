package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.sponsorship.CancelSponsorshipTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json.{Format, JsObject, Json}

case class CancelSponsorshipRequest(version: Option[Byte] = None,
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
    extends TxRequest.For[CancelSponsorshipTransaction] {

  protected def sign(tx: CancelSponsorshipTransaction, signer: PrivateKeyAccount): CancelSponsorshipTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, CancelSponsorshipTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      tx <- CancelSponsorshipTransaction.create(
        version.getOrElse(CancelSponsorshipTransaction.latestVersion),
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

object CancelSponsorshipRequest {
  implicit val jsonFormat: Format[CancelSponsorshipRequest] = Format(
    Json.reads[CancelSponsorshipRequest],
    Json.writes[CancelSponsorshipRequest].transform((json: JsObject) => Json.obj("type" -> CancelSponsorshipTransaction.typeId.toInt) ++ json)
  )
}
