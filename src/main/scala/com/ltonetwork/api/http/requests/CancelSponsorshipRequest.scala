package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.sponsorship.CancelSponsorshipTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json.{Format, JsObject, Json}

case class CancelSponsorshipRequest(version: Option[Byte] = None,
                                    timestamp: Option[Long] = None,
                                    senderKeyType: Option[String] = None,
                                    senderPublicKey: Option[String] = None,
                                    fee: Long,
                                    recipient: String,
                                    sponsorKeyType: Option[String] = None,
                                    sponsorPublicKey: Option[String] = None,
                                    signature: Option[ByteStr] = None,
                                    proofs: Option[Proofs] = None)
    extends TxRequest.For[CancelSponsorshipTransaction] {

  protected def sign(tx: CancelSponsorshipTransaction, signer: PrivateKeyAccount): CancelSponsorshipTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount,
               sponsor: Option[PublicKeyAccount],
               time: Option[Time]): Either[ValidationError, CancelSponsorshipTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      tx <- CancelSponsorshipTransaction.create(
        version.getOrElse(CancelSponsorshipTransaction.latestVersion),
        None,
        timestamp(time),
        sender,
        fee,
        validRecipient,
        sponsor,
        validProofs
      )
    } yield tx
}

object CancelSponsorshipRequest {
  implicit val jsonFormat: Format[CancelSponsorshipRequest] = Format(
    Json.reads[CancelSponsorshipRequest],
    Json.writes[CancelSponsorshipRequest].transform((json: JsObject) => Json.obj("type" -> CancelSponsorshipTransaction.typeId.toInt) ++ json)
  )
}
