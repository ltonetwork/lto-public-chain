package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.transfer.MassTransferTransaction
import com.ltonetwork.transaction.transfer.MassTransferTransaction.Transfer
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json}

case class MassTransferRequest(version: Option[Byte] = None,
                               timestamp: Option[Long] = None,
                               senderKeyType: Option[String] = None,
                               senderPublicKey: Option[String] = None,
                               fee: Long,
                               transfers: List[Transfer],
                               attachment: Option[ByteStr] = None,
                               sponsorKeyType: Option[String] = None,
                               sponsorPublicKey: Option[String] = None,
                               signature: Option[ByteStr] = None,
                               proofs: Option[Proofs] = None)
    extends TxRequest.For[MassTransferTransaction] {

  protected def sign(tx: MassTransferTransaction, signer: PrivateKeyAccount): MassTransferTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, MassTransferTransaction] =
    for {
      validTransfers <- MassTransferTransaction.parseTransfersList(transfers)
      validProofs    <- toProofs(signature, proofs)
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
}

object MassTransferRequest {
  implicit val jsonFormat: Format[MassTransferRequest] = Format(
    Json.reads[MassTransferRequest],
    Json.writes[MassTransferRequest].transform((json: JsObject) => Json.obj("type" -> MassTransferTransaction.typeId.toInt) ++ json)
  )
}
