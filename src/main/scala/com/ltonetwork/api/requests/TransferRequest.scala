package com.ltonetwork.api.requests

import com.ltonetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.transfer.TransferTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json._

case class TransferRequest(version: Option[Byte] = None,
                           timestamp: Option[Long] = None,
                           fee: Long,
                           sender: Option[String] = None,
                           senderKeyType: Option[String] = None,
                           senderPublicKey: Option[String] = None,
                           sponsor: Option[String] = None,
                           sponsorKeyType: Option[String] = None,
                           sponsorPublicKey: Option[String] = None,
                           recipient: String,
                           amount: Long,
                           attachment: Option[ByteStr] = None,
                           signature: Option[ByteStr] = None,
                           proofs: Option[Proofs] = None)
    extends TxRequest.For[TransferTransaction] {

  protected def sign(tx: TransferTransaction, signer: PrivateKeyAccount): TransferTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, TransferTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      tx <- TransferTransaction.create(
        version.getOrElse(TransferTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        validRecipient,
        amount,
        attachment.getOrElse(ByteStr.empty).arr,
        sponsor,
        proofs
      )
    } yield tx
}

object TransferRequest {
  implicit val jsonFormat: Format[TransferRequest] = Format(
    Json.reads[TransferRequest],
    Json.writes[TransferRequest].transform((json: JsObject) => Json.obj("type" -> TransferTransaction.typeId.toInt) ++ json)
  )
}
