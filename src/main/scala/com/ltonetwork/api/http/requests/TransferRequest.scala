package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.transfer.TransferTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json._

case class TransferRequest(version: Option[Byte],
                           timestamp: Option[Long] = None,
                           sender: Option[String],
                           senderPublicKey: Option[String],
                           fee: Long,
                           recipient: String,
                           amount: Long,
                           attachment: Option[ByteStr] = None,
                           signature: Option[ByteStr] = None,
                           proofs: Option[Proofs] = None
    ) extends TxRequest {

  def toTx(sender: PublicKeyAccount): Either[ValidationError, TransferTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      tx <- TransferTransaction.create(
        version.getOrElse(TransferTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        amount,
        attachment.getOrElse(ByteStr.empty).arr,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransferTransaction] = for {
    senderAddress  <- sender.toRight(GenericError("invalid.sender"))
    senderAccount  <- wallet.findPrivateKey(senderAddress)
    signerAccount  <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    validRecipient <- Address.fromString(recipient)
    tx <- TransferTransaction.signed(
      version.getOrElse(TransferTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validRecipient,
      amount,
      attachment.getOrElse(ByteStr.empty).arr,
      signerAccount
    )
  } yield tx}

object TransferRequest {
  implicit val jsonFormat: Format[TransferRequest] = Json.format
}
