package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.transfer.TransferTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json._

case class TransferRequest(version: Option[Byte] = None,
                           timestamp: Option[Long] = None,
                           senderKeyType: Option[String] = None,
                           senderPublicKey: Option[String] = None,
                           fee: Long,
                           recipient: String,
                           amount: Long,
                           attachment: Option[ByteStr] = None,
                           sponsorKeyType: Option[String] = None,
                           sponsorPublicKey: Option[String] = None,
                           signature: Option[ByteStr] = None,
                           proofs: Option[Proofs] = None)
    extends TxRequest.For[TransferTransaction] {

  protected def sign(tx: TransferTransaction, signer: PrivateKeyAccount): TransferTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], time: Option[Time]): Either[ValidationError, TransferTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      tx <- TransferTransaction.create(
        version.getOrElse(TransferTransaction.latestVersion),
        None,
        timestamp(time),
        sender,
        fee,
        validRecipient,
        amount,
        attachment.getOrElse(ByteStr.empty).arr,
        sponsor,
        validProofs
      )
    } yield tx
}

object TransferRequest {
  implicit val jsonFormat: Format[TransferRequest] = Format(
    Json.reads[TransferRequest],
    Json.writes[TransferRequest].transform((json: JsObject) => Json.obj("type" -> TransferTransaction.typeId.toInt) ++ json)
  )
}
