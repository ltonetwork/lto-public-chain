package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.{ByteStr, DataEntry}
import com.ltonetwork.transaction.association.IssueAssociationTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class IssueAssociationRequest(version: Option[Byte] = None,
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
                                   expires: Option[Long] = None,
                                   subject: Option[ByteStr] = None,
                                   data: Option[List[DataEntry[_]]] = None,
                                   signature: Option[ByteStr] = None,
                                   proofs: Option[Proofs] = None)
    extends TxRequest.For[IssueAssociationTransaction] {

  protected def sign(tx: IssueAssociationTransaction, signer: PrivateKeyAccount): IssueAssociationTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, IssueAssociationTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      tx <- IssueAssociationTransaction.create(
        version.getOrElse(IssueAssociationTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        associationType,
        validRecipient,
        expires,
        subject.noneIfEmpty,
        data.getOrElse(List.empty[DataEntry[_]]),
        sponsor,
        proofs
      )
    } yield tx
}

object IssueAssociationRequest {
  implicit val jsonFormat: Format[IssueAssociationRequest] = Format(
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
      (JsPath \ "expires").readNullable[Long] and
      (JsPath \ "subject").readNullable[ByteStr].orElse((JsPath \ "hash").readNullable[ByteStr]) and
      (JsPath \ "data").readNullable[List[DataEntry[_]]] and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(IssueAssociationRequest.apply _),
    Json.writes[IssueAssociationRequest].transform((json: JsObject) => Json.obj("type" -> IssueAssociationTransaction.typeId.toInt) ++ json)
  )
}
