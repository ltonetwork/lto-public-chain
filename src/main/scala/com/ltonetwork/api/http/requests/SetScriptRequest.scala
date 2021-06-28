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

case class SetScriptRequest(version: Option[Byte],
                            timestamp: Option[Long] = None,
                            sender: Option[String],
                            senderPublicKey: Option[String],
                            fee: Long,
                            script: Option[String],
                            signature: Option[ByteStr] = None,
                            proofs: Option[Proofs] = None
                           ) extends TxRequest[SetScriptTransaction] {

  private def decodedScript: Either[ValidationError, Option[Script]] = script match {
    case None    => Right(None)
    case Some(s) => Script.fromBase64String(s).map(Some(_))
  }

  def toTx(sender: PublicKeyAccount): Either[ValidationError, SetScriptTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      validScript <- decodedScript
      tx <- SetScriptTransaction.create(
        version.getOrElse(SetScriptTransaction.latestVersion),
        None,
        timestamp.getOrElse(defaultTimestamp),
        sender,
        fee,
        validScript,
        None,
        validProofs
      )
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, SetScriptTransaction] = for {
    senderAddress <- sender.toRight(GenericError("invalid.sender"))
    senderAccount <- wallet.findPrivateKey(senderAddress)
    signerAccount <- if (senderAddress == signerAddress) Right(senderAccount) else wallet.findPrivateKey(signerAddress)
    validScript   <- decodedScript
    tx <- SetScriptTransaction.signed(
      version.getOrElse(SetScriptTransaction.latestVersion),
      timestamp.getOrElse(time.getTimestamp()),
      senderAccount,
      fee,
      validScript,
      signerAccount
    )
  } yield tx
}

object SetScriptRequest {
  implicit val jsonFormat: Format[SetScriptRequest] = Json.format[SetScriptRequest]
}
