package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.sponsorship.CancelSponsorshipTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, Json}

case class CancelSponsorshipRequest(version: Option[Byte],
                                    timestamp: Option[Long] = None,
                                    sender: Option[String],
                                    senderPublicKey: Option[String],
                                    fee: Long,
                                    recipient: String,
                                    signature: Option[ByteStr] = None,
                                    proofs: Option[Proofs] = None
                                   ) extends TxRequest {

  def toTx(sender: PublicKeyAccount): Either[ValidationError, CancelSponsorshipTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs <- toProofs(signature, proofs)
      tx <- CancelSponsorshipTransaction.create(
        version.getOrElse(CancelSponsorshipTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, CancelSponsorshipTransaction] = for {
    senderAddress  <- sender.toRight(GenericError("invalid.sender"))
    senderAccount  <- wallet.findPrivateKey(senderAddress)
    signerAccount  <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    validRecipient <- Address.fromString(recipient)
    tx <- CancelSponsorshipTransaction.signed(
      version.getOrElse(CancelSponsorshipTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validRecipient,
      signerAccount
    )
  } yield tx
}

object CancelSponsorshipRequest {
  implicit val jsonFormat: Format[CancelSponsorshipRequest] = Json.format
}
