package com.ltonetwork.api.http.requests

import cats.implicits._
import com.ltonetwork.account.KeyTypes.{ED25519, SECP256K1, SECP256R1}
import com.ltonetwork.account.{KeyType, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, JsError, JsNumber, JsObject, JsPath, JsString, JsSuccess, Json, OWrites, Reads, Writes}

case class AnchorRequest(version: Option[Byte] = None,
                         timestamp: Option[Long] = None,
                         sender: Option[String] = None,
                         senderKeyType: Option[String] = None,
                         senderPublicKey: Option[String] = None,
                         fee: Long,
                         anchors: List[String],
                         sponsor: Option[String] = None,
                         sponsorKeyType: Option[String],
                         sponsorPublicKey: Option[String] = None,
                         signature: Option[ByteStr] = None,
                         proofs: Option[Proofs] = None,
    ) extends TxRequest[AnchorTransaction] {

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, AnchorTransaction] =
    for {
      validAnchors <- anchors.traverse(s => parseBase58(s, "invalid anchor", AnchorTransaction.MaxAnchorStringSize))
      validProofs  <- toProofs(signature, proofs)
      tx <- AnchorTransaction.create(
        version.getOrElse(AnchorTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validAnchors,
        sponsor,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, AnchorTransaction] = for {
    accounts       <- resolveAccounts(wallet, signerAddress)
    (senderAccount, sponsorAccount, signerAccount) = accounts
    validAnchors   <- anchors.traverse(s => parseBase58(s, "invalid anchor", AnchorTransaction.MaxAnchorStringSize))
    validProofs    <- toProofs(signature, proofs)
    tx <- AnchorTransaction.signed(
      version.getOrElse(AnchorTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validAnchors,
      sponsorAccount,
      validProofs,
      signerAccount
    )
  } yield tx
}

object AnchorRequest {
  implicit val jsonFormat: Format[AnchorRequest] = Format(
    Json.reads[AnchorRequest],
    Json.writes[AnchorRequest].transform((json: JsObject) => Json.obj("type" -> AnchorTransaction.typeId.toInt) ++ json)
  )
}
