package com.ltonetwork.api.http.requests.signed

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.state.DataEntry
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}

@ApiModel(value = "Signed Data transaction")
case class SignedDataRequest(@ApiModelProperty(required = true)
                             version: Byte,
                             @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                             senderPublicKey: String,
                             @ApiModelProperty(value = "Data to put into blockchain", required = true)
                             data: List[DataEntry[_]],
                             @ApiModelProperty(required = true)
                             fee: Long,
                             @ApiModelProperty(required = true)
                             timestamp: Long,
                             @ApiModelProperty(required = true)
                             proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, DataTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- DataTransaction.create(version, _sender, data, fee, timestamp, _proofs)
    } yield t
}
