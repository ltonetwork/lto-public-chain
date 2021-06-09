package com.ltonetwork.api.http.requests.lease

import com.ltonetwork.account.{AddressOrAlias, PublicKeyAccount}
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.TransactionBuilders.SignatureStringLength
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}

@ApiModel(value = "Signed Lease transaction")
case class SignedLeaseV1Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
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
                                signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseTransaction] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _recipient <- AddressOrAlias.fromString(recipient)
      _t         <- LeaseTransaction.create(1, None, timestamp, _sender, fee, _recipient, amount, None, Proofs.fromSignature(_signature))
    } yield _t
}

object SignedLeaseV1Request {
  implicit val broadcastLeaseRequestReadsFormat: Format[SignedLeaseV1Request] = Json.format
}
