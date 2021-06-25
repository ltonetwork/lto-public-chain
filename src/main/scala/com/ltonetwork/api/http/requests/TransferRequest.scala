package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.transaction.transfer.TransferTransaction
import play.api.libs.json._

case class TransferRequest(version: Option[Byte],
                           sender: Option[String],
                           senderPublicKey: Option[String],
                           recipient: String,
                           amount: Long,
                           fee: Long,
                           attachment: Option[Array[Byte]] = None,
                           timestamp: Option[Long] = None,
                           signature: Option[ByteStr] = None,
                           proofs: Option[Proofs] = None
    ) extends TxRequest {

  def toTxFrom(sender: PublicKeyAccount): Either[ValidationError, TransferTransaction] =
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
        attachment.getOrElse(Array.empty[Byte]),
        None,
        validProofs
      )
    } yield tx
}

object TransferRequest {
  implicit val jsonFormat: Format[TransferRequest] = Json.format
}
