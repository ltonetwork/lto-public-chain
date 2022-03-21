package com.ltonetwork.api.requests

import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.burn.BurnTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json}

case class BurnRequest(version: Option[Byte] = None,
                         timestamp: Option[Long] = None,
                         fee: Long,
                         sender: Option[String] = None,
                         senderKeyType: Option[String] = None,
                         senderPublicKey: Option[String] = None,
                         sponsor: Option[String] = None,
                         sponsorKeyType: Option[String] = None,
                         sponsorPublicKey: Option[String] = None,
                         amount: Long = 0,
                         signature: Option[ByteStr] = None,
                         proofs: Option[Proofs] = None)
  extends TxRequest.For[BurnTransaction] {

  protected def sign(tx: BurnTransaction, signer: PrivateKeyAccount): BurnTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, BurnTransaction] =
    for {
      tx <- BurnTransaction.create(
        version.getOrElse(BurnTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        amount,
        sponsor,
        proofs
      )
    } yield tx
}

object BurnRequest {
  implicit val jsonFormat: Format[BurnRequest] = Format(
    Json.reads[BurnRequest],
    Json.writes[BurnRequest].transform((json: JsObject) => Json.obj("type" -> BurnTransaction.typeId.toInt) ++ json)
  )
}
