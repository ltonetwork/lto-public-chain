package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.association.RevokeAssociationTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class RevokeAssociationRequest(version: Option[Byte] = None,
                                    timestamp: Option[Long] = None,
                                    fee: Long,
                                    sender: Option[String] = None,
                                    senderKeyType: Option[String] = None,
                                    senderPublicKey: Option[String] = None,
                                    sponsor: Option[String] = None,
                                    sponsorKeyType: Option[String] = None,
                                    sponsorPublicKey: Option[String] = None,
                                    recipient: String,
                                    associationType: Int,
                                    hash: Option[ByteStr] = None,
                                    signature: Option[ByteStr] = None,
                                    proofs: Option[Proofs] = None,
) extends TxRequest.For[RevokeAssociationTransaction] {

  protected def sign(tx: RevokeAssociationTransaction, signer: PrivateKeyAccount): RevokeAssociationTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, RevokeAssociationTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      tx <- RevokeAssociationTransaction.create(
        version.getOrElse(RevokeAssociationTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        validRecipient,
        associationType,
        hash.noneIfEmpty,
        sponsor,
        proofs
      )
    } yield tx
}

object RevokeAssociationRequest {
  implicit val jsonFormat: Format[RevokeAssociationRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderKeyType").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "sponsor").readNullable[String] and
      (JsPath \ "sponsorKeyType").readNullable[String] and
      (JsPath \ "sponsorPublicKey").readNullable[String] and
      (JsPath \ "recipient").read[String].orElse((JsPath \ "party").read[String]) and
      (JsPath \ "associationType").read[Int].orElse((JsPath \ "assocType").read[Int]) and
      (JsPath \ "subject").readNullable[ByteStr].orElse((JsPath \ "hash").readNullable[ByteStr]) and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(RevokeAssociationRequest.apply _),
    Json.writes[RevokeAssociationRequest].transform((json: JsObject) => Json.obj("type" -> RevokeAssociationTransaction.typeId.toInt) ++ json)
  )
}
