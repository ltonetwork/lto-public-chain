package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json}

case class LeaseRequest(version: Option[Byte] = None,
                        timestamp: Option[Long] = None,
                        fee: Long,
                        sender: Option[String] = None,
                        senderKeyType: Option[String] = None,
                        senderPublicKey: Option[String] = None,
                        sponsor: Option[String] = None,
                        sponsorKeyType: Option[String] = None,
                        sponsorPublicKey: Option[String] = None,
                        recipient: String,
                        amount: Long,
                        signature: Option[ByteStr] = None,
                        proofs: Option[Proofs] = None)
    extends TxRequest.For[LeaseTransaction] {

  protected def sign(tx: LeaseTransaction, signer: PrivateKeyAccount): LeaseTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, LeaseTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      tx <- LeaseTransaction.create(
        version.getOrElse(LeaseTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        validRecipient,
        amount,
        sponsor,
        proofs
      )
    } yield tx
}

object LeaseRequest {
  implicit val jsonFormat: Format[LeaseRequest] = Format(
    Json.reads[LeaseRequest],
    Json.writes[LeaseRequest].transform((json: JsObject) => Json.obj("type" -> LeaseTransaction.typeId.toInt) ++ json)
  )
}
