package com.ltonetwork.api.http

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.Validation
import com.ltonetwork.transaction.{AnchorTransaction, Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json, Writes}

object AnchorRequest {
  implicit val unsignedDataRequestReads = Json.reads[AnchorRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedAnchorRequest]

}

case class AnchorRequest(version: Byte, sender: String, anchors: List[String], fee: Long, timestamp: Option[Long] = None)

@ApiModel(value = "Signed Data transaction")
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
  def toTx: Either[ValidationError, AnchorTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _anchors    <- anchors.traverse(s => parseBase58(s, "invalid anchor", Proofs.MaxAnchorStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- AnchorTransaction.create(version, _sender, _anchors, fee, timestamp, _proofs)
    } yield t
}