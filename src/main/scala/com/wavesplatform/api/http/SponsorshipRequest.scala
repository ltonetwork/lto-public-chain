package com.wavesplatform.api.http

import cats.implicits._
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.transaction.{Proofs, SponsorshipTransactionBase, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json

object SponsorshipRequest {
  implicit val unsignedDataRequestReads = Json.reads[SponsorshipRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedSponsorshipRequest]
}

case class SponsorshipRequest(version: Byte,
                              sender: String,
                              recipient: String,
                              fee: Long,
                              timestamp: Option[Long] = None)

@ApiModel(value = "Signed Data transaction")
case class SignedSponsorshipRequest(@ApiModelProperty(required = true)
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
  def toTx[T](ctor: SponsorshipTransactionBase.CreateCtor[T]): Either[ValidationError, T] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _recipient      <- Address.fromString(recipient)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- ctor(version, _sender, _recipient, fee, timestamp, _proofs)
    } yield t
}
