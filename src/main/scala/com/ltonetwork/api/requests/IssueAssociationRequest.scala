package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.association.IssueAssociationTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class IssueAssociationRequest(version: Option[Byte] = None,
                                   timestamp: Option[Long] = None,
                                   senderKeyType: Option[String] = None,
                                   senderPublicKey: Option[String] = None,
                                   fee: Long,
                                   recipient: String,
                                   associationType: Int,
                                   expires: Option[Long] = None,
                                   hash: Option[ByteStr] = None,
                                   sponsorKeyType: Option[String] = None,
                                   sponsorPublicKey: Option[String] = None,
                                   signature: Option[ByteStr] = None,
                                   proofs: Option[Proofs] = None,
) extends TxRequest.For[IssueAssociationTransaction] {

  protected def sign(tx: IssueAssociationTransaction, signer: PrivateKeyAccount): IssueAssociationTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount,
               sponsor: Option[PublicKeyAccount],
               time: Option[Time]): Either[ValidationError, IssueAssociationTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      tx <- IssueAssociationTransaction.create(
        version.getOrElse(IssueAssociationTransaction.latestVersion),
        None,
        timestamp(time),
        sender,
        fee,
        validRecipient,
        associationType,
        expires,
        hash.noneIfEmpty,
        sponsor,
        validProofs
      )
    } yield tx
}

object IssueAssociationRequest {
  implicit val jsonFormat: Format[IssueAssociationRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "senderKeyType").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "recipient").read[String].orElse((JsPath \ "party").read[String]) and
      (JsPath \ "associationType").read[Int] and
      (JsPath \ "expires").readNullable[Long] and
      (JsPath \ "hash").readNullable[ByteStr] and
      (JsPath \ "sponsorKeyType").readNullable[String] and
      (JsPath \ "sponsorPublicKey").readNullable[String] and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(IssueAssociationRequest.apply _),
    Json.writes[IssueAssociationRequest].transform((json: JsObject) => Json.obj("type" -> IssueAssociationTransaction.typeId.toInt) ++ json)
  )
}
