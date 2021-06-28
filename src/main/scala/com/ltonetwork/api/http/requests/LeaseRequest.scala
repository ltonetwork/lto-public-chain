package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, Json}

case class LeaseRequest(version: Option[Byte],
                        timestamp: Option[Long] = None,
                        sender: Option[String],
                        senderPublicKey: Option[String],
                        fee: Long,
                        recipient: String,
                        amount: Long,
                        signature: Option[ByteStr] = None,
                        proofs: Option[Proofs] = None
    ) extends TxRequest[LeaseTransaction] {

  def toTx(sender: PublicKeyAccount): Either[ValidationError, LeaseTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      tx <- LeaseTransaction.create(
        version.getOrElse(LeaseTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        amount,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseTransaction] = for {
    senderAddress <- sender.toRight(GenericError("invalid.sender"))
    senderAccount <- wallet.findPrivateKey(senderAddress)
    signerAccount <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    validRecipient <- Address.fromString(recipient)
    tx <- LeaseTransaction.signed(
      version.getOrElse(LeaseTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validRecipient,
      amount,
      signerAccount
    )
  } yield tx
}

object LeaseRequest {
  implicit val jsonFormat: Format[LeaseRequest] = Json.format
}
