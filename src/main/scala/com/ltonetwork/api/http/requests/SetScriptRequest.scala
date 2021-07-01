package com.ltonetwork.api.http.requests

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.{Format, Json}

case class SetScriptRequest(version: Option[Byte] = None,
                            timestamp: Option[Long] = None,
                            sender: Option[String] = None,
                            senderPublicKey: Option[String] = None,
                            fee: Long,
                            script: Option[String],
                            sponsor: Option[String] = None,
                            sponsorPublicKey: Option[String] = None,
                            signature: Option[ByteStr] = None,
                            proofs: Option[Proofs] = None
                           ) extends TxRequest[SetScriptTransaction] {

  private def decodedScript: Either[ValidationError, Option[Script]] = script match {
    case None    => Right(None)
    case Some(s) => Script.fromBase64String(s).map(Some(_))
  }

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, SetScriptTransaction] =
    for {
      validScript <- decodedScript
      validProofs <- toProofs(signature, proofs)
      tx <- SetScriptTransaction.create(
        version.getOrElse(SetScriptTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validScript,
        sponsor,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, SetScriptTransaction] = for {
    accounts       <- resolveAccounts(wallet, signerAddress)
    (senderAccount, sponsorAccount, signerAccount) = accounts
    validScript   <- decodedScript
    validProofs    <- toProofs(signature, proofs)
    tx <- SetScriptTransaction.signed(
      version.getOrElse(SetScriptTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validScript,
      sponsorAccount,
      validProofs,
      signerAccount
    )
  } yield tx
}

object SetScriptRequest {
  implicit val jsonFormat: Format[SetScriptRequest] = Json.format[SetScriptRequest]
}
