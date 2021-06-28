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

case class MassTransferRequest(version: Option[Byte],
                               timestamp: Option[Long] = None,
                               sender: Option[String],
                               senderPublicKey: Option[String],
                               fee: Long,
                               transfers: List[Transfer],
                               attachment: Option[ByteStr] = None,
                               signature: Option[ByteStr] = None,
                               proofs: Option[Proofs] = None
    ) extends TxRequest {

  def toTx(sender: PublicKeyAccount): Either[ValidationError, MassTransferTransaction] =
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
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, MassTransferTransaction] = for {
    senderAddress  <- sender.toRight(GenericError("invalid.sender"))
    senderAccount  <- wallet.findPrivateKey(senderAddress)
    signerAccount  <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    validTransfers <- MassTransferTransaction.parseTransfersList(transfers)
    tx <- MassTransferTransaction.signed(
      version.getOrElse(MassTransferTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validTransfers,
      attachment.getOrElse(ByteStr.empty).arr,
      signerAccount
    )
  } yield tx
}

object MassTransferRequest {
  implicit val jsonFormat: Format[MassTransferRequest] = Json.format
}
