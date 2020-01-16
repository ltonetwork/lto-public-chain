package com.wavesplatform.api.http

import cats.implicits._
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.transaction.{AssociationTransaction, Proofs, RevokeAssociationTransaction, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json

object RevokeAssociationRequest {
  implicit val unsignedDataRequestReads = Json.reads[RevokeAssociationRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedRevokeAssociationRequest]
}

case class RevokeAssociationRequest(version: Byte,
                                    sender: String,
                                    party: String,
                                    associationType: Int,
                                    hash: String = "",
                                    fee: Long,
                                    timestamp: Option[Long] = None)

@ApiModel(value = "Signed Data transaction")
case class SignedRevokeAssociationRequest(@ApiModelProperty(required = true)
                                          version: Byte,
                                          @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                          senderPublicKey: String,
                                          @ApiModelProperty(value = "Counterparty address", required = true)
                                          party: String,
                                          @ApiModelProperty(value = "Association type", required = true)
                                          associationType: Int,
                                          @ApiModelProperty(value = "Association data hash ", required = false)
                                          hash: String = "",
                                          @ApiModelProperty(required = true)
                                          fee: Long,
                                          @ApiModelProperty(required = true)
                                          timestamp: Long,
                                          @ApiModelProperty(required = true)
                                          proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, RevokeAssociationTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _party      <- Address.fromString(party)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _hash       <- if (hash == "") Right(None) else parseBase58(hash, "Incorrect hash", AssociationTransaction.StringHashLength).map(Some(_))
      _proofs     <- Proofs.create(_proofBytes)
      t <- RevokeAssociationTransaction.create(version,
                                               _sender,
                                               _party,
                                               associationType,
                                               _hash.map(AnchorRequest.prependZeros),
                                               fee,
                                               timestamp,
                                               _proofs)
    } yield t
}
