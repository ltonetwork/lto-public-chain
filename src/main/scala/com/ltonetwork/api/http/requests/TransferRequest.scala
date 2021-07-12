package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, KeyType, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.transfer.TransferTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json._

case class TransferRequest(version: Option[Byte] = None,
                           timestamp: Option[Long] = None,
                           sender: Option[String] = None,
                           senderKeyType: Option[String] = None,
                           senderPublicKey: Option[String] = None,
                           fee: Long,
                           recipient: String,
                           amount: Long,
                           attachment: Option[ByteStr] = None,
                           sponsor: Option[String] = None,
                           sponsorKeyType: Option[String] = None,
                           sponsorPublicKey: Option[String] = None,
                           signature: Option[ByteStr] = None,
                           proofs: Option[Proofs] = None
    ) extends TxRequest[TransferTransaction] {

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, TransferTransaction] =
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
        sponsor,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransferTransaction] = for {
    accounts       <- resolveAccounts(wallet, signerAddress)
    (senderAccount, sponsorAccount, signerAccount) = accounts
    validRecipient <- Address.fromString(recipient)
    validProofs    <- toProofs(signature, proofs)
    tx <- TransferTransaction.signed(
      version.getOrElse(TransferTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validRecipient,
      amount,
      attachment.getOrElse(ByteStr.empty).arr,
      sponsorAccount,
      validProofs,
      signerAccount
    )
  } yield tx}

object TransferRequest {
  implicit val jsonFormat: Format[TransferRequest] = Format(
    Json.reads[TransferRequest],
    Json.writes[TransferRequest].transform((json: JsObject) => Json.obj("type" -> TransferTransaction.typeId.toInt) ++ json)
  )
}
