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
                              senderKeyType: Option[String] = None,
                              senderPublicKey: Option[String] = None,
                              fee: Long,
                              leaseId: ByteStr,
                              sponsorKeyType: Option[String] = None,
                              sponsorPublicKey: Option[String] = None,
                              signature: Option[ByteStr] = None,
                              proofs: Option[Proofs] = None)
    extends TxRequest.For[CancelLeaseTransaction] {

  protected def sign(tx: CancelLeaseTransaction, signer: PrivateKeyAccount): CancelLeaseTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], time: Option[Time]): Either[ValidationError, CancelLeaseTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      tx <- CancelLeaseTransaction.create(
        version.getOrElse(CancelLeaseTransaction.latestVersion),
        None,
        timestamp(time),
        sender,
        fee,
        leaseId,
        sponsor,
        validProofs
      )
    } yield tx
}

object CancelLeaseRequest {
  implicit val jsonFormat: Format[CancelLeaseRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "senderKeyType").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "leaseId").read[ByteStr].orElse((JsPath \ "txId").read[ByteStr]) and
      (JsPath \ "sponsorKeyType").readNullable[String] and
      (JsPath \ "sponsorPublicKey").readNullable[String] and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(CancelLeaseRequest.apply _),
    Json.writes[CancelLeaseRequest].transform((json: JsObject) => Json.obj("type" -> CancelLeaseTransaction.typeId.toInt) ++ json)
  )
}
