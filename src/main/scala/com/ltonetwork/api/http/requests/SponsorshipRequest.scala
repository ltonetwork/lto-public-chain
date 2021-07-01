package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.sponsorship.SponsorshipTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, Json}

case class SponsorshipRequest(version: Option[Byte] = None,
                              timestamp: Option[Long] = None,
                              sender: Option[String] = None,
                              senderPublicKey: Option[String] = None,
                              fee: Long,
                              recipient: String,
                              sponsor: Option[String] = None,
                              sponsorPublicKey: Option[String] = None,
                              signature: Option[ByteStr] = None,
                              proofs: Option[Proofs] = None
    ) extends TxRequest[SponsorshipTransaction] {

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, SponsorshipTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      tx <- SponsorshipTransaction.create(
        version.getOrElse(SponsorshipTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        sponsor,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, SponsorshipTransaction] = for {
    accounts       <- resolveAccounts(wallet, signerAddress)
    (senderAccount, sponsorAccount, signerAccount) = accounts
    validRecipient <- Address.fromString(recipient)
    validProofs    <- toProofs(signature, proofs)
    tx <- SponsorshipTransaction.signed(
      version.getOrElse(SponsorshipTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validRecipient,
      sponsorAccount,
      validProofs,
      signerAccount
    )
  } yield tx
}

object SponsorshipRequest {
  implicit val jsonFormat: Format[SponsorshipRequest] = Json.format
}
