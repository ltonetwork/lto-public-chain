package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, Json}

case class LeaseRequest(version: Option[Byte],
                        sender: Option[String],
                        senderPublicKey: Option[String],
                        recipient: String,
                        amount: Long,
                        fee: Long,
                        timestamp: Option[Long] = None,
                        signature: Option[ByteStr] = None,
                        proofs: Option[Proofs] = None
    ) extends TxRequest {

  def toTxFrom(sender: PublicKeyAccount): Either[ValidationError, LeaseTransaction] =
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
        None,
        validProofs
      )
    } yield tx
}

object LeaseRequest {
  implicit val jsonFormat: Format[LeaseRequest] = Json.format
}
