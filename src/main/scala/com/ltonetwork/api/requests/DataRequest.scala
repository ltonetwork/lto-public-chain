package com.ltonetwork.api.requests

import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.{ByteStr, DataEntry}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json.{Format, JsObject, Json}

case class DataRequest(version: Option[Byte] = None,
                       timestamp: Option[Long] = None,
                       senderKeyType: Option[String] = None,
                       senderPublicKey: Option[String] = None,
                       fee: Long,
                       data: List[DataEntry[_]],
                       sponsorKeyType: Option[String] = None,
                       sponsorPublicKey: Option[String] = None,
                       signature: Option[ByteStr] = None,
                       proofs: Option[Proofs] = None)
    extends TxRequest.For[DataTransaction] {

  protected def sign(tx: DataTransaction, signer: PrivateKeyAccount): DataTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], time: Option[Time]): Either[ValidationError, DataTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      tx <- DataTransaction.create(
        version.getOrElse(DataTransaction.latestVersion),
        None,
        timestamp(time),
        sender,
        fee,
        data,
        sponsor,
        validProofs
      )
    } yield tx
}

object DataRequest {
  implicit val jsonFormat: Format[DataRequest] = Format(
    Json.reads[DataRequest],
    Json.writes[DataRequest].transform((json: JsObject) => Json.obj("type" -> DataTransaction.typeId.toInt) ++ json)
  )
}
