package com.ltonetwork.api.http.requests.data

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.state.DataEntry
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json

object SignedDataV1Request {
  implicit val reads = Json.reads[SignedDataV1Request]
}

@ApiModel(value = "Signed Data transaction")
case class SignedDataV1Request(@ApiModelProperty(required = true)
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
      t           <- DataTransaction.create(version, None, timestamp, _sender, fee, data, None, _proofs)
    } yield t
}
