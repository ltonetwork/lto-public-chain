package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction.claim.ClaimTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json}

case class ClaimRequest(version: Option[Byte] = None,
                         timestamp: Option[Long] = None,
                         senderKeyType: Option[String] = None,
                         senderPublicKey: Option[String] = None,
                         fee: Long,
                         claimType: Int,
                         recipient: Option[String],
                         subject: Option[ByteStr] = None,
                         amount: Option[Long],
                         hash: Option[ByteStr] = None,
                         related: Option[List[ByteStr]] = None,
                         sponsorKeyType: Option[String] = None,
                         sponsorPublicKey: Option[String] = None,
                         signature: Option[ByteStr] = None,
                         proofs: Option[Proofs] = None,
                        ) extends TxRequest.For[ClaimTransaction] {

  protected def sign(tx: ClaimTransaction, signer: PrivateKeyAccount): ClaimTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, ClaimTransaction] =
    for {
      validRecipient <- recipient.noneIfEmpty.map(r => Address.fromString(r))
        .fold[Either[ValidationError, Option[Address]]](Right(None))(_.map(Some(_)))
      validProofs  <- toProofs(signature, proofs)
      tx <- ClaimTransaction.create(
        version.getOrElse(ClaimTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        claimType,
        validRecipient,
        subject.noneIfEmpty,
        amount.getOrElse(0),
        hash.noneIfEmpty,
        related.getOrElse(List.empty[ByteStr]),
        sponsor,
        validProofs
      )
    } yield tx
}

object ClaimRequest {
  implicit val jsonFormat: Format[ClaimRequest] = Format(
    Json.reads[ClaimRequest],
    Json.writes[ClaimRequest].transform((json: JsObject) => Json.obj("type" -> ClaimTransaction.typeId.toInt) ++ json)
  )
}
