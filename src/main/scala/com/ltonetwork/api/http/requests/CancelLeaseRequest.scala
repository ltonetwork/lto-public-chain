package com.ltonetwork.api.http.requests

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.lease.CancelLeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class CancelLeaseRequest(version: Option[Byte],
                              timestamp: Option[Long],
                              sender: Option[String],
                              senderPublicKey: Option[String],
                              fee: Long,
                              leaseId: String,
                              signature: Option[ByteStr],
                              proofs: Option[Proofs]
    ) extends TxRequest[CancelLeaseTransaction] {

  def toTx(sender: PublicKeyAccount): Either[ValidationError, CancelLeaseTransaction] =
    for {
      validProofs  <- toProofs(signature, proofs)
      validLeaseId <- parseBase58(leaseId, "invalid.leaseTx", DigestStringLength)
      tx <- CancelLeaseTransaction.create(
        version.getOrElse(CancelLeaseTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validLeaseId,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, CancelLeaseTransaction] = for {
    senderAddress <- sender.toRight(GenericError("invalid.sender"))
    senderAccount <- wallet.findPrivateKey(senderAddress)
    signerAccount <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    validLeaseId  <- parseBase58(leaseId, "invalid.leaseTx", DigestStringLength)
    tx <- CancelLeaseTransaction.signed(
      version.getOrElse(CancelLeaseTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validLeaseId,
      signerAccount
    )
  } yield tx
}

object CancelLeaseRequest {
  import com.ltonetwork.utils.byteStrFormat
  implicit val jsonFormat: Format[CancelLeaseRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "leaseId").read[String].orElse((JsPath \ "txId").read[String]) and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(CancelLeaseRequest.apply _),
    Json.writes[CancelLeaseRequest]
  )
}
