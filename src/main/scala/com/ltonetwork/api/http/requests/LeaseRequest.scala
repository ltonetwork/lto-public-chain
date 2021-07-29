package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json}

case class LeaseRequest(version: Option[Byte] = None,
                        timestamp: Option[Long] = None,
                        senderKeyType: Option[String] = None,
                        senderPublicKey: Option[String] = None,
                        fee: Long,
                        recipient: String,
                        amount: Long,
                        sponsorKeyType: Option[String] = None,
                        sponsorPublicKey: Option[String] = None,
                        signature: Option[ByteStr] = None,
                        proofs: Option[Proofs] = None
    ) extends TxRequest.For[LeaseTransaction] {

  protected def sign(tx: LeaseTransaction, signer: PrivateKeyAccount): LeaseTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, LeaseTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      tx <- LeaseTransaction.create(
        version.getOrElse(LeaseTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        amount,
        sponsor,
        validProofs
      )
    } yield tx
}

object LeaseRequest {
  implicit val jsonFormat: Format[LeaseRequest] = Format(
    Json.reads[LeaseRequest],
    Json.writes[LeaseRequest].transform((json: JsObject) => Json.obj("type" -> LeaseTransaction.typeId.toInt) ++ json)
  )
}
