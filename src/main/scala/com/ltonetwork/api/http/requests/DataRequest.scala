package com.ltonetwork.api.http.requests

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.{ByteStr, DataEntry}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, JsObject, Json}

case class DataRequest(version: Option[Byte] = None,
                       timestamp: Option[Long] = None,
                       sender: Option[String] = None,
                       senderPublicKey: Option[String] = None,
                       fee: Long,
                       data: List[DataEntry[_]],
                       sponsor: Option[String] = None,
                       sponsorPublicKey: Option[String] = None,
                       signature: Option[ByteStr] = None,
                       proofs: Option[Proofs] = None
    ) extends TxRequest[DataTransaction] {

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, DataTransaction] =
    for {
      validProofs    <- toProofs(signature, proofs)
      tx <- DataTransaction.create(
        version.getOrElse(DataTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        data,
        sponsor,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, DataTransaction] = for {
    accounts       <- resolveAccounts(wallet, signerAddress)
    (senderAccount, sponsorAccount, signerAccount) = accounts
    validProofs    <- toProofs(signature, proofs)
    tx <- DataTransaction.signed(
      version.getOrElse(DataTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      data,
      sponsorAccount,
      validProofs,
      signerAccount
    )
  } yield tx
}

object DataRequest {
  implicit val jsonFormat: Format[DataRequest] = Format(
    Json.reads[DataRequest],
    Json.writes[DataRequest].transform((json: JsObject) => Json.obj("type" -> DataTransaction.typeId.toInt) ++ json)
  )
}
