package com.ltonetwork.api.http

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.transaction.anchor.AnchorTransactionV1
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json, Writes}

object AnchorRequest {
  implicit val unsignedDataRequestReads = Json.reads[AnchorRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedAnchorRequest]

}

case class AnchorRequest(version: Byte, sender: String, anchors: List[String], fee: Long, timestamp: Option[Long] = None)

@ApiModel(value = "Signed Anchor transaction")
case class SignedAnchorRequest(@ApiModelProperty(required = true)
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
  def toTx: Either[ValidationError, AnchorTransactionV1] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _anchors    <- anchors.traverse(s => parseBase58(s, "invalid anchor", Proofs.MaxAnchorStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- AnchorTransactionV1.create(version, timestamp, _sender, fee, _anchors, None, _proofs)
    } yield t
}
