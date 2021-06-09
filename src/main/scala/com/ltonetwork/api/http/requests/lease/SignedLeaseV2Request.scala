package com.ltonetwork.api.http.requests.lease

import cats.implicits._
import com.ltonetwork.account.{AddressOrAlias, PublicKeyAccount}
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}

@ApiModel(value = "Signed Lease transaction")
case class SignedLeaseV2Request(@ApiModelProperty(required = true)
                                version: Byte,
                                @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                senderPublicKey: String,
                                @ApiModelProperty(required = true)
                                amount: Long,
                                @ApiModelProperty(required = true)
                                fee: Long,
                                @ApiModelProperty(value = "Recipient address", required = true)
                                recipient: String,
                                @ApiModelProperty(required = true)
                                timestamp: Long,
                                @ApiModelProperty(required = true)
                                proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _recipient  <- AddressOrAlias.fromString(recipient)
      _t          <- LeaseTransaction.create(version, None, timestamp, _sender, fee, _recipient, amount, None, _proofs)
    } yield _t
}

object SignedLeaseV2Request {
  implicit val broadcastLeaseRequestReadsFormat: Format[SignedLeaseV2Request] = Json.format
}
