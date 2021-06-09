package com.ltonetwork.api.http.requests.lease

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.TransactionBuilders.SignatureStringLength
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.transaction.lease.CancelLeaseTransaction
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json._

@ApiModel(value = "Signed Cancel Lease transaction")
case class SignedCancelLeaseV1Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                      senderPublicKey: String,
                                      @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                      txId: String,
                                      @ApiModelProperty(required = true)
                                      timestamp: Long,
                                      @ApiModelProperty(required = true)
                                      signature: String,
                                      @ApiModelProperty(required = true)
                                      fee: Long)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CancelLeaseTransaction] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _leaseTx   <- parseBase58(txId, "invalid.leaseTx", SignatureStringLength)
      _t         <- CancelLeaseTransaction.create(1, None, timestamp, _sender, fee, _leaseTx, None, Proofs.fromSignature(_signature))
    } yield _t
}

object SignedCancelLeaseV1Request {
  implicit val reads: Reads[SignedCancelLeaseV1Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "txId").read[String].orElse((JsPath \ "leaseId").read[String]) and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String] and
      (JsPath \ "fee").read[Long]
  )(SignedCancelLeaseV1Request.apply _)

  implicit val writes: Writes[SignedCancelLeaseV1Request] = Json.writes[SignedCancelLeaseV1Request]
}
