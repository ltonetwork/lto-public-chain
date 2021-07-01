package com.ltonetwork.api.http.requests

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.transfer.MassTransferTransaction
import com.ltonetwork.transaction.transfer.MassTransferTransaction.Transfer
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, Json}

case class MassTransferRequest(version: Option[Byte] = None,
                               timestamp: Option[Long] = None,
                               sender: Option[String] = None,
                               senderPublicKey: Option[String] = None,
                               fee: Long,
                               transfers: List[Transfer],
                               attachment: Option[ByteStr] = None,
                               sponsor: Option[String] = None,
                               sponsorPublicKey: Option[String] = None,
                               signature: Option[ByteStr] = None,
                               proofs: Option[Proofs] = None
    ) extends TxRequest[MassTransferTransaction] {

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, MassTransferTransaction] =
    for {
      validTransfers <- MassTransferTransaction.parseTransfersList(transfers)
      validProofs <- toProofs(signature, proofs)
      tx <- MassTransferTransaction.create(
        version.getOrElse(MassTransferTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validTransfers,
        attachment.getOrElse(ByteStr.empty).arr,
        sponsor,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, MassTransferTransaction] = for {
    accounts       <- resolveAccounts(wallet, signerAddress)
    (senderAccount, sponsorAccount, signerAccount) = accounts
    validTransfers <- MassTransferTransaction.parseTransfersList(transfers)
    validProofs <- toProofs(signature, proofs)
    tx <- MassTransferTransaction.signed(
      version.getOrElse(MassTransferTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validTransfers,
      attachment.getOrElse(ByteStr.empty).arr,
      sponsorAccount,
      validProofs,
      signerAccount
    )
  } yield tx
}

object MassTransferRequest {
  implicit val jsonFormat: Format[MassTransferRequest] = Json.format
}
