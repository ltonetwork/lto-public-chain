package one.legalthings.api.http.assets

import cats.implicits._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Json, OFormat}
import one.legalthings.account.PublicKeyAccount
import one.legalthings.api.http.BroadcastRequest
import one.legalthings.transaction.smart.SetScriptTransaction
import one.legalthings.transaction.smart.script.Script
import one.legalthings.transaction.{Proofs, ValidationError}

object SignedSetScriptRequest {
  implicit val jsonFormat: OFormat[SignedSetScriptRequest] = Json.format
}

@ApiModel(value = "Proven SetScript transaction")
case class SignedSetScriptRequest(@ApiModelProperty(required = true)
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
      t           <- SetScriptTransaction.create(version, _sender, _script, fee, timestamp, _proofs)
    } yield t
}
