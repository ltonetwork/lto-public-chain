package one.legalthings.api.http.assets

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import one.legalthings.account.{AddressOrAlias, PublicKeyAccount}
import one.legalthings.api.http.BroadcastRequest
import one.legalthings.transaction.TransactionParsers.SignatureStringLength
import one.legalthings.transaction.transfer._
import one.legalthings.transaction.ValidationError

object SignedTransferV1Request {
  implicit val reads: Reads[SignedTransferV1Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "signature").read[String]
  )(SignedTransferV1Request.apply _)

  implicit val writes: Writes[SignedTransferV1Request] = Json.writes[SignedTransferV1Request]
}

@ApiModel(value = "Signed Asset transfer transaction")
case class SignedTransferV1Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                   senderPublicKey: String,
                                   @ApiModelProperty(value = "Recipient address", required = true)
                                   recipient: String,
                                   @ApiModelProperty(required = true, example = "1000000")
                                   amount: Long,
                                   @ApiModelProperty(required = true)
                                   fee: Long,
                                   @ApiModelProperty(required = true)
                                   timestamp: Long,
                                   @ApiModelProperty(value = "Base58 encoded attachment")
                                   attachment: Option[String],
                                   @ApiModelProperty(required = true)
                                   signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, TransferTransactionV1] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature  <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      _account    <- AddressOrAlias.fromString(recipient)
      t           <- TransferTransactionV1.create(_sender, _account, amount, timestamp, fee, _attachment.arr, _signature)
    } yield t
}
