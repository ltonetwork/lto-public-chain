package com.ltonetwork.api.http.requests

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.transfer.MassTransferTransaction
import com.ltonetwork.transaction.transfer.MassTransferTransaction.Transfer
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, Json}

case class MassTransferRequest(version: Option[Byte],
                               sender: Option[String],
                               senderPublicKey: Option[String],
                               transfers: List[Transfer],
                               fee: Long,
                               attachment: Option[Array[Byte]] = None,
                               timestamp: Option[Long] = None,
                               signature: Option[ByteStr] = None,
                               proofs: Option[Proofs] = None
    ) extends TxRequest {

  def toTxFrom(sender: PublicKeyAccount): Either[ValidationError, MassTransferTransaction] =
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
        attachment.getOrElse(Array.empty[Byte]),
        None,
        validProofs
      )
    } yield tx
}

object MassTransferRequest {
  implicit val jsonFormat: Format[MassTransferRequest] = Json.format
}
