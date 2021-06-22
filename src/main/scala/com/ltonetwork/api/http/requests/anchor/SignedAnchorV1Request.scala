package com.ltonetwork.api.http.requests.anchor

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}

@ApiModel(value = "Signed Anchor transaction")
case class SignedAnchorV1Request(@ApiModelProperty(required = true)
                               version: Byte,
                                 @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                               senderPublicKey: String,
                                 @ApiModelProperty(value = "Anchors to put into blockchain", required = true)
                               anchors: List[String],
                                 @ApiModelProperty(required = true)
                               fee: Long,
                                 @ApiModelProperty(required = true)
                               timestamp: Long,
                                 @ApiModelProperty(required = true)
                               proofs: List[String])
  extends BroadcastRequest {
  def toTx: Either[ValidationError, AnchorTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _anchors    <- anchors.traverse(s => parseBase58(s, "invalid anchor", AnchorTransaction.MaxAnchorStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- AnchorTransaction.create(version, None, timestamp, _sender, fee, _anchors, None, _proofs)
    } yield t
}
