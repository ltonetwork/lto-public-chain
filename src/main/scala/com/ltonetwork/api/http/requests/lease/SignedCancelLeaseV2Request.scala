package com.ltonetwork.api.http.requests.lease

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.TransactionBuilders.SignatureStringLength
import com.ltonetwork.transaction.lease.CancelLeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json._

@ApiModel(value = "Signed Cancel Lease transaction")
case class SignedCancelLeaseV2Request(@ApiModelProperty(required = true)
                                      version: Byte,
                                      @ApiModelProperty(required = true)
                                      chainId: Byte,
                                      @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                      senderPublicKey: String,
                                      @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                      leaseId: String,
                                      @ApiModelProperty(required = true)
                                      timestamp: Long,
                                      @ApiModelProperty(required = true)
                                      proofs: List[String],
                                      @ApiModelProperty(required = true)
                                      fee: Long)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CancelLeaseTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _leaseTx    <- parseBase58(leaseId, "invalid.leaseTx", SignatureStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _t          <- CancelLeaseTransaction.create(version, Some(chainId), timestamp, _sender, fee, _leaseTx, None, _proofs)
    } yield _t
}
object SignedCancelLeaseV2Request {
  implicit val reads: Reads[SignedCancelLeaseV2Request] = (
    (JsPath \ "version").read[Byte] and
      (JsPath \ "chainId").read[Byte] and
      (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "leaseId").read[String] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").read[List[String]] and
      (JsPath \ "fee").read[Long]
  )(SignedCancelLeaseV2Request.apply _)

  implicit val writes = Json.writes[SignedCancelLeaseV2Request]
}
