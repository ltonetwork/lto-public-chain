package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction.claim.ClaimTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json}

case class ClaimRequest(version: Option[Byte] = None,
                        fee: Long,
                        timestamp: Option[Long] = None,
                        sender: Option[String] = None,
                        senderKeyType: Option[String] = None,
                        senderPublicKey: Option[String] = None,
                        sponsor: Option[String] = None,
                        sponsorKeyType: Option[String] = None,
                        sponsorPublicKey: Option[String] = None,
                        claimType: Long,
                        recipient: Option[String],
                        subject: Option[ByteStr] = None,
                        data: Option[List[DataEntry[_]]],
                        signature: Option[ByteStr] = None,
                        proofs: Option[Proofs] = None)
    extends TxRequest.For[ClaimTransaction] {

  protected def sign(tx: ClaimTransaction, signer: PrivateKeyAccount): ClaimTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, ClaimTransaction] =
    for {
      validRecipient <- recipient.noneIfEmpty.map(r => Address.fromString(r))
        .fold[Either[ValidationError, Option[Address]]](Right(None))(_.map(Some(_)))
      tx <- ClaimTransaction.create(
        version.getOrElse(ClaimTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        claimType,
        validRecipient,
        subject.noneIfEmpty,
        data.getOrElse(List.empty[DataEntry[_]]),
        sponsor,
        proofs
      )
    } yield tx
}

object ClaimRequest {
  implicit val jsonFormat: Format[ClaimRequest] = Format(
    Json.reads[ClaimRequest],
    Json.writes[ClaimRequest].transform((json: JsObject) => Json.obj("type" -> ClaimTransaction.typeId.toInt) ++ json)
  )
}
