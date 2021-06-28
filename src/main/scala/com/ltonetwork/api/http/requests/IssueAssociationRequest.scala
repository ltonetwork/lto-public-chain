package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.association.IssueAssociationTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class IssueAssociationRequest(version: Option[Byte],
                                   timestamp: Option[Long] = None,
                                   sender: Option[String],
                                   senderPublicKey: Option[String],
                                   fee: Long,
                                   recipient: String,
                                   associationType: Int,
                                   expires: Option[Long] = None,
                                   hash: Option[ByteStr] = None,
                                   signature: Option[ByteStr] = None,
                                   proofs: Option[Proofs] = None,
    ) extends TxRequest[IssueAssociationTransaction] {

  def toTx(sender: PublicKeyAccount): Either[ValidationError, IssueAssociationTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      validRecipient <- Address.fromString(recipient)
      tx <- IssueAssociationTransaction.create(
        version.getOrElse(IssueAssociationTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        associationType,
        expires,
        hash.noneIfEmpty,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, IssueAssociationTransaction] = for {
    senderAddress <- sender.toRight(GenericError("invalid.sender"))
    senderAccount <- wallet.findPrivateKey(senderAddress)
    signerAccount <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    validRecipient <- Address.fromString(recipient)
    tx <- IssueAssociationTransaction.signed(
      version.getOrElse(IssueAssociationTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validRecipient,
      associationType,
      expires,
      hash.noneIfEmpty,
      signerAccount
    )
  } yield tx
}

object IssueAssociationRequest {
  implicit val jsonFormat: Format[IssueAssociationRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "recipient").read[String].orElse((JsPath \ "party").read[String]) and
      (JsPath \ "associationType").read[Int] and
      (JsPath \ "expires").readNullable[Long] and
      (JsPath \ "hash").readNullable[ByteStr] and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(IssueAssociationRequest.apply _),
    Json.writes[CancelLeaseRequest]
  )
}
