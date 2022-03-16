package com.ltonetwork.api.requests

import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.lease.CancelLeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class CancelLeaseRequest(version: Option[Byte] = None,
                              timestamp: Option[Long] = None,
                              fee: Long,
                              sender: Option[String] = None,
                              senderKeyType: Option[String] = None,
                              senderPublicKey: Option[String] = None,
                              sponsor: Option[String] = None,
                              sponsorKeyType: Option[String] = None,
                              sponsorPublicKey: Option[String] = None,
                              leaseId: ByteStr,
                              signature: Option[ByteStr] = None,
                              proofs: Option[Proofs] = None)
    extends TxRequest.For[CancelLeaseTransaction] {

  protected def sign(tx: CancelLeaseTransaction, signer: PrivateKeyAccount): CancelLeaseTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, CancelLeaseTransaction] =
    for {
      tx <- CancelLeaseTransaction.create(
        version.getOrElse(CancelLeaseTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        leaseId,
        sponsor,
        proofs
      )
    } yield tx
}

object CancelLeaseRequest {
  implicit val jsonFormat: Format[CancelLeaseRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderKeyType").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "sponsor").readNullable[String] and
      (JsPath \ "sponsorKeyType").readNullable[String] and
      (JsPath \ "sponsorPublicKey").readNullable[String] and
      (JsPath \ "leaseId").read[ByteStr].orElse((JsPath \ "txId").read[ByteStr]) and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(CancelLeaseRequest.apply _),
    Json.writes[CancelLeaseRequest].transform((json: JsObject) => Json.obj("type" -> CancelLeaseTransaction.typeId.toInt) ++ json)
  )
}
