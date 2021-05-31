package com.ltonetwork.api.http.requests.signed

import cats.implicits._
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.sponsorship.SponsorshipTransactionBase
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}

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
      _recipient  <- Address.fromString(recipient)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- ctor(version, _sender, _recipient, fee, timestamp, _proofs)
    } yield t
}
