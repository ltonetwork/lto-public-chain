package com.ltonetwork.api.http.requests

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, Json}

case class AnchorRequest(version: Option[Byte],
                         timestamp: Option[Long] = None,
                         sender: Option[String],
                         senderPublicKey: Option[String],
                         fee: Long,
                         anchors: List[String],
                         signature: Option[ByteStr] = None,
                         proofs: Option[Proofs] = None,
    ) extends TxRequest {

  def toTx(sender: PublicKeyAccount): Either[ValidationError, AnchorTransaction] =
    for {
      validProofs  <- toProofs(signature, proofs)
      validAnchors <- anchors.traverse(s => parseBase58(s, "invalid anchor", AnchorTransaction.MaxAnchorStringSize))
      tx <- AnchorTransaction.create(
        version.getOrElse(AnchorTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validAnchors,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, AnchorTransaction] = for {
    senderAddress <- sender.toRight(GenericError("invalid.sender"))
    senderAccount <- wallet.findPrivateKey(senderAddress)
    signerAccount <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    validAnchors  <- anchors.traverse(s => parseBase58(s, "invalid anchor", AnchorTransaction.MaxAnchorStringSize))
    tx <- AnchorTransaction.signed(
      version.getOrElse(AnchorTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validAnchors,
      signerAccount
    )
  } yield tx
}

object AnchorRequest {
  implicit val jsonFormat: Format[AnchorRequest] = Json.format[AnchorRequest]
}
