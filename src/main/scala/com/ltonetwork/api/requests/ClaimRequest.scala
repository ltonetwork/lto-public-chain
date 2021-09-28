package com.ltonetwork.api.requests

import cats.implicits._
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.claim.ClaimTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, JsNumber, JsObject, Json, OWrites}

case class ClaimRequest(version: Option[Byte] = None,
                         timestamp: Option[Long] = None,
                         senderKeyType: Option[String] = None,
                         senderPublicKey: Option[String] = None,
                         fee: Long,
                         anchors: List[String],
                         sponsorKeyType: Option[String] = None,
                         sponsorPublicKey: Option[String] = None,
                         signature: Option[ByteStr] = None,
                         proofs: Option[Proofs] = None,
                        ) extends TxRequest.For[ClaimTransaction] {

  protected def sign(tx: ClaimTransaction, signer: PrivateKeyAccount): ClaimTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, ClaimTransaction] =
    for {
      validClaims <- anchors.traverse(s => parseBase58(s, "invalid anchor", ClaimTransaction.MaxClaimStringSize))
      validProofs  <- toProofs(signature, proofs)
      tx <- ClaimTransaction.create(
        version.getOrElse(ClaimTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validClaims,
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
