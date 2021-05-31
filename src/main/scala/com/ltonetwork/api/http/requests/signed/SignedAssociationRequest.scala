package com.ltonetwork.api.http.requests.signed

import cats.implicits._
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.association.AssociationTransaction
import com.ltonetwork.transaction.{Proofs, TransactionFactory, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}

@ApiModel(value = "Signed Association transaction")
case class SignedAssociationRequest(@ApiModelProperty(required = true)
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
  def toTx[T](ctor: AssociationTransaction.CreateCtor[T]): Either[ValidationError, T] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _party      <- Address.fromString(party)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _hash <- if (hash == "") Right(None)
      else parseBase58(hash, TransactionFactory.IncorectHashMessage, AssociationTransaction.StringHashLength).map(Some(_))
      _proofs <- Proofs.create(_proofBytes)
      t       <- ctor(version, _sender, _party, associationType, _hash, fee, timestamp, _proofs)
    } yield t
}
