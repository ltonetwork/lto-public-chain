package com.ltonetwork.api.http.requests

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.{ByteStr, DataEntry}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, Json}

case class DataRequest(version: Option[Byte],
                       timestamp: Option[Long] = None,
                       sender: Option[String],
                       senderPublicKey: Option[String],
                       fee: Long,
                       data: List[DataEntry[_]],
                       signature: Option[ByteStr] = None,
                       proofs: Option[Proofs] = None
    ) extends TxRequest[DataTransaction] {

  def toTx(sender: PublicKeyAccount): Either[ValidationError, DataTransaction] =
    for {
      validProofs    <- toProofs(signature, proofs)
      tx <- DataTransaction.create(
        version.getOrElse(DataTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        data,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, DataTransaction] = for {
    senderAddress  <- sender.toRight(GenericError("invalid.sender"))
    senderAccount  <- wallet.findPrivateKey(senderAddress)
    signerAccount  <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    tx <- DataTransaction.signed(
      version.getOrElse(DataTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      data,
      signerAccount
    )
  } yield tx
}

object DataRequest {
  implicit val jsonFormat: Format[DataRequest] = Json.format[DataRequest]
}
