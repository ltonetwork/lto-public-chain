package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, JsObject, Json}

case class LeaseRequest(version: Option[Byte] = None,
                        timestamp: Option[Long] = None,
                        sender: Option[String] = None,
                        senderPublicKey: Option[String] = None,
                        fee: Long,
                        recipient: String,
                        amount: Long,
                        sponsor: Option[String] = None,
                        sponsorPublicKey: Option[String] = None,
                        signature: Option[ByteStr] = None,
                        proofs: Option[Proofs] = None
    ) extends TxRequest[LeaseTransaction] {

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, LeaseTransaction] =
    for {
      validRecipient <- Address.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      tx <- LeaseTransaction.create(
        version.getOrElse(LeaseTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validRecipient,
        amount,
        sponsor,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseTransaction] = for {
    accounts       <- resolveAccounts(wallet, signerAddress)
    (senderAccount, sponsorAccount, signerAccount) = accounts
    validRecipient <- Address.fromString(recipient)
    validProofs    <- toProofs(signature, proofs)
    tx <- LeaseTransaction.signed(
      version.getOrElse(LeaseTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validRecipient,
      amount,
      sponsorAccount,
      validProofs,
      signerAccount
    )
  } yield tx
}

object LeaseRequest {
  implicit val jsonFormat: Format[LeaseRequest] = Format(
    Json.reads[LeaseRequest],
    Json.writes[LeaseRequest].transform((json: JsObject) => Json.obj("type" -> LeaseTransaction.typeId.toInt) ++ json)
  )
}
