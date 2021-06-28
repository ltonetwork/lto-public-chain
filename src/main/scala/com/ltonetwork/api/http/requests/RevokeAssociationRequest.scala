package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.association.RevokeAssociationTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class RevokeAssociationRequest(version: Option[Byte],
                                    timestamp: Option[Long] = None,
                                    sender: Option[String],
                                    senderPublicKey: Option[String],
                                    fee: Long,
                                    recipient: String,
                                    associationType: Int,
                                    hash: Option[ByteStr] = None,
                                    signature: Option[ByteStr] = None,
                                    proofs: Option[Proofs] = None,
    ) extends TxRequest {

  def toTx(sender: PublicKeyAccount): Either[ValidationError, RevokeAssociationTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      validRecipient <- Address.fromString(recipient)
      tx <- RevokeAssociationTransaction.create(
        version.getOrElse(RevokeAssociationTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        associationType,
        hash.noneIfEmpty,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, RevokeAssociationTransaction] = for {
    senderAddress <- sender.toRight(GenericError("invalid.sender"))
    senderAccount <- wallet.findPrivateKey(senderAddress)
    signerAccount <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    validRecipient <- Address.fromString(recipient)
    tx <- RevokeAssociationTransaction.signed(
      version.getOrElse(RevokeAssociationTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validRecipient,
      associationType,
      hash.noneIfEmpty,
      signerAccount
    )
  } yield tx
}

object RevokeAssociationRequest {
  implicit val jsonFormat: Format[RevokeAssociationRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "recipient").read[String].orElse((JsPath \ "party").read[String]) and
      (JsPath \ "associationType").read[Int] and
      (JsPath \ "hash").readNullable[ByteStr] and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(RevokeAssociationRequest.apply _),
    Json.writes[CancelLeaseRequest]
  )
}