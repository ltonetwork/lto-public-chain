package com.ltonetwork.api.http.requests

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.lease.CancelLeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class CancelLeaseRequest(version: Option[Byte],
                              sender: Option[String],
                              senderPublicKey: Option[String],
                              leaseId: String,
                              fee: Long,
                              timestamp: Option[Long],
                              signature: Option[ByteStr],
                              proofs: Option[Proofs]
    ) extends TxRequest {

  def toTxFrom(sender: PublicKeyAccount): Either[ValidationError, CancelLeaseTransaction] =
    for {
      validProofs  <- toProofs(signature, proofs)
      validLeaseId <- parseBase58(leaseId, "invalid.leaseTx", DigestStringLength)
      tx <- CancelLeaseTransaction.create(
        version.getOrElse(CancelLeaseTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validLeaseId,
        None,
        validProofs
      )
    } yield tx
}

object CancelLeaseRequest {
  import com.ltonetwork.utils.byteStrFormat
  implicit val jsonFormat: Format[CancelLeaseRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "leaseId").read[String].orElse((JsPath \ "txId").read[String]) and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(CancelLeaseRequest.apply _),
    Json.writes[CancelLeaseRequest]
  )
}
