package com.ltonetwork.api.http.requests.smart

import cats.implicits._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Json, OFormat}

object SignedSetScriptV1Request {
  implicit val jsonFormat: OFormat[SignedSetScriptV1Request] = Json.format
}

@ApiModel(value = "Signed SetScript transaction")
case class SignedSetScriptV1Request(@ApiModelProperty(required = true)
                                    version: Byte,
                                    @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                    senderPublicKey: String,
                                    @ApiModelProperty(value = "Base64 encoded script(including version and checksum)", required = true)
                                    script: Option[String],
                                    @ApiModelProperty(required = true)
                                    fee: Long,
                                    @ApiModelProperty(required = true)
                                    timestamp: Long,
                                    @ApiModelProperty(required = true)
                                    proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, SetScriptTransaction] =
    for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _script <- script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- SetScriptTransaction.create(version, None, timestamp, _sender, fee, _script, None, _proofs)
    } yield t
}
