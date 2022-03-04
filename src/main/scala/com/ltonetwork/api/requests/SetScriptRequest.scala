package com.ltonetwork.api.requests

import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.{Proofs, ValidationError}
import com.ltonetwork.utils.Time
import play.api.libs.json.{Format, JsObject, Json, OWrites}

case class SetScriptRequest(version: Option[Byte] = None,
                            timestamp: Option[Long] = None,
                            fee: Long,
                            sender: Option[String] = None,
                            senderKeyType: Option[String] = None,
                            senderPublicKey: Option[String] = None,
                            sponsor: Option[String] = None,
                            sponsorKeyType: Option[String] = None,
                            sponsorPublicKey: Option[String] = None,
                            script: Option[String],
                            signature: Option[ByteStr] = None,
                            proofs: Option[Proofs] = None)
    extends TxRequest.For[SetScriptTransaction] {

  private def decodedScript: Either[ValidationError, Option[Script]] = script match {
    case None                  => Right(None)
    case Some(s) if s.isEmpty  => Right(None)
    case Some(s) if s.nonEmpty => Script.fromBase64String(s).map(Some(_))
  }

  protected def sign(tx: SetScriptTransaction, signer: PrivateKeyAccount): SetScriptTransaction = tx.signWith(signer)

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, SetScriptTransaction] =
    for {
      validScript <- decodedScript
      tx <- SetScriptTransaction.create(
        version.getOrElse(SetScriptTransaction.latestVersion),
        None,
        timestamp,
        sender,
        fee,
        validScript,
        sponsor,
        proofs
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
