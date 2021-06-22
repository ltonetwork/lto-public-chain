package com.ltonetwork.api.http.requests.sponsorship

import cats.implicits._
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.sponsorship.CancelSponsorshipTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}

@ApiModel(value = "Signed Cancel Sponsorship transaction")
case class SignedCancelSponsorshipV1Request(@ApiModelProperty(required = true)
                                            version: Byte,
                                            @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                            senderPublicKey: String,
                                            @ApiModelProperty(value = "Counterparty address", required = true)
                                            recipient: String,
                                            @ApiModelProperty(required = true)
                                            fee: Long,
                                            @ApiModelProperty(required = true)
                                            timestamp: Long,
                                            @ApiModelProperty(required = true)
                                            proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CancelSponsorshipTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _recipient  <- Address.fromString(recipient)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- CancelSponsorshipTransaction.create(version, None, timestamp, _sender, fee, _recipient, None, _proofs)
    } yield t
}
