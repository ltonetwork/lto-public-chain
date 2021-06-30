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

case class CancelLeaseRequest(version: Option[Byte] = None,
                              timestamp: Option[Long] = None,
                              sender: Option[String] = None,
                              senderPublicKey: Option[String] = None,
                              fee: Long,
                              leaseId: ByteStr,
                              signature: Option[ByteStr] = None,
                              proofs: Option[Proofs] = None
    ) extends TxRequest[CancelLeaseTransaction] {

  def toTxFrom(sender: PublicKeyAccount): Either[ValidationError, CancelLeaseTransaction] =
    for {
      validProofs  <- toProofs(signature, proofs)
      tx <- CancelLeaseTransaction.create(
        version.getOrElse(CancelLeaseTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        leaseId,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, CancelLeaseTransaction] = for {
    senderAddress <- sender.toRight(GenericError("invalid.sender"))
    senderAccount <- wallet.findPrivateKey(senderAddress)
    signerAccount <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    tx <- CancelLeaseTransaction.signed(
      version.getOrElse(CancelLeaseTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      leaseId,
      signerAccount
    )
  } yield tx
}

object CancelLeaseRequest {
  implicit val jsonFormat: Format[CancelLeaseRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "leaseId").read[ByteStr].orElse((JsPath \ "txId").read[ByteStr]) and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(CancelLeaseRequest.apply _),
    Json.writes[CancelLeaseRequest]
  )
}
