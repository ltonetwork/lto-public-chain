package com.ltonetwork.api.http.requests.signed

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.transfer.MassTransferTransaction.Transfer
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json._

object SignedMassTransferRequest {
  implicit val reads = Json.reads[SignedMassTransferRequest]
}

@ApiModel(value = "Signed MassTransfer transaction")
case class SignedMassTransferRequest(@ApiModelProperty(required = true)
                                     version: Byte,
                                     @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                     senderPublicKey: String,
                                     @ApiModelProperty(value = "List of (recipient, amount) pairs", required = true)
                                     transfers: List[Transfer],
                                     @ApiModelProperty(required = true)
                                     fee: Long,
                                     @ApiModelProperty(required = true)
                                     timestamp: Long,
                                     @ApiModelProperty(value = "Base58 encoded attachment")
                                     attachment: Option[String],
                                     @ApiModelProperty(required = true)
                                     proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, MassTransferTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      _transfers  <- MassTransferTransaction.parseTransfersList(transfers)
      t           <- MassTransferTransaction.create(version, _sender, _transfers, timestamp, fee, _attachment.arr, _proofs)
    } yield t
}