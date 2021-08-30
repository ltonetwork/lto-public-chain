package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.{Proofs, ValidationError}
import play.api.libs.json.{Format, JsObject, Json, OWrites}

case class SetScriptRequest(version: Option[Byte] = None,
                            timestamp: Option[Long] = None,
                            senderKeyType: Option[String] = None,
                            senderPublicKey: Option[String] = None,
                            fee: Long,
                            script: Option[String],
                            sponsorKeyType: Option[String] = None,
                            sponsorPublicKey: Option[String] = None,
                            signature: Option[ByteStr] = None,
                            proofs: Option[Proofs] = None)
    extends TxRequest.For[SetScriptTransaction] {

  private def decodedScript: Either[ValidationError, Option[Script]] = script match {
    case None    => Right(None)
    case Some(s) => Script.fromBase64String(s).map(Some(_))
  }

  protected def sign(tx: SetScriptTransaction, signer: PrivateKeyAccount): SetScriptTransaction = tx.signWith(signer)

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
}

object SetScriptRequest {
  implicit val jsonFormat: Format[SetScriptRequest] = Format(
    Json.reads[SetScriptRequest],
    Json.writes[SetScriptRequest].transform((json: JsObject) => Json.obj("type" -> SetScriptTransaction.typeId.toInt) ++ json)
  )

  // Needed for compiling the tests. But why?
  implicit val jsonWrites: OWrites[SetScriptRequest] = Json.writes[SetScriptRequest]
}
