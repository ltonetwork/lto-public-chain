package com.ltonetwork.api.requests

import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.{ByteStr, DataEntry}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json.{Format, JsObject, Json}

case class DataRequest(version: Option[Byte] = None,
                       timestamp: Option[Long] = None,
                       fee: Long,
                       sender: Option[String] = None,
                       senderKeyType: Option[String] = None,
                       senderPublicKey: Option[String] = None,
                       sponsor: Option[String] = None,
                       sponsorKeyType: Option[String] = None,
                       sponsorPublicKey: Option[String] = None,
                       data: List[DataEntry[_]],
                       signature: Option[ByteStr] = None,
                       proofs: Option[Proofs] = None)
    extends TxRequest.For[DataTransaction] {

  protected def sign(tx: DataTransaction, signer: PrivateKeyAccount): DataTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, DataTransaction] =
    for {
      tx <- DataTransaction.create(
        version.getOrElse(DataTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        data,
        sponsor,
        proofs
      )
    } yield tx
}

object DataRequest {
  implicit val jsonFormat: Format[DataRequest] = Format(
    Json.reads[DataRequest],
    Json.writes[DataRequest].transform((json: JsObject) => Json.obj("type" -> DataTransaction.typeId.toInt) ++ json)
  )
}
