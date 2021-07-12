package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, KeyType, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.association.RevokeAssociationTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class RevokeAssociationRequest(version: Option[Byte] = None,
                                    timestamp: Option[Long] = None,
                                    sender: Option[String] = None,
                                    senderKeyType: Option[String] = None,
                                    senderPublicKey: Option[String] = None,
                                    fee: Long,
                                    recipient: String,
                                    associationType: Int,
                                    hash: Option[ByteStr] = None,
                                    sponsor: Option[String] = None,
                                    sponsorKeyType: Option[String] = None,
                                    sponsorPublicKey: Option[String] = None,
                                    signature: Option[ByteStr] = None,
                                    proofs: Option[Proofs] = None,
    ) extends TxRequest[RevokeAssociationTransaction] {

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, RevokeAssociationTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs <- toProofs(signature, proofs)
      tx <- RevokeAssociationTransaction.create(
        version.getOrElse(RevokeAssociationTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        associationType,
        hash.noneIfEmpty,
        sponsor,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, RevokeAssociationTransaction] = for {
    accounts       <- resolveAccounts(wallet, signerAddress)
    (senderAccount, sponsorAccount, signerAccount) = accounts
    validRecipient <- Address.fromString(recipient)
    validProofs <- toProofs(signature, proofs)
    tx <- RevokeAssociationTransaction.signed(
      version.getOrElse(RevokeAssociationTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validRecipient,
      associationType,
      hash.noneIfEmpty,
      sponsorAccount,
      validProofs,
      signerAccount
    )
  } yield tx
}

object RevokeAssociationRequest {
  implicit val jsonFormat: Format[RevokeAssociationRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderKeyType").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "recipient").read[String].orElse((JsPath \ "party").read[String]) and
      (JsPath \ "associationType").read[Int] and
      (JsPath \ "hash").readNullable[ByteStr] and
      (JsPath \ "sponsor").readNullable[String] and
      (JsPath \ "sponsorKeyType").readNullable[String] and
      (JsPath \ "sponsorPublicKey").readNullable[String] and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(RevokeAssociationRequest.apply _),
    Json.writes[RevokeAssociationRequest].transform((json: JsObject) => Json.obj("type" -> RevokeAssociationTransaction.typeId.toInt) ++ json)
  )
}