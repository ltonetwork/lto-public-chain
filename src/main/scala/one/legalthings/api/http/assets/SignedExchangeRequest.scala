package one.legalthings.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import one.legalthings.account.PublicKeyAccount
import one.legalthings.api.http.BroadcastRequest
import one.legalthings.transaction.TransactionParsers.SignatureStringLength
import one.legalthings.transaction.ValidationError
import one.legalthings.transaction.assets.exchange.{ExchangeTransaction, Order}

object SignedExchangeRequest {
  implicit val orderFormat: Format[Order]                                 = one.legalthings.transaction.assets.exchange.OrderJson.orderFormat
  implicit val signedExchangeRequestFormat: Format[SignedExchangeRequest] = Json.format
}

case class SignedExchangeRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                 senderPublicKey: String,
                                 @ApiModelProperty(value = "Buy Order")
                                 order1: Order,
                                 @ApiModelProperty(value = "Sell Order")
                                 order2: Order,
                                 @ApiModelProperty(required = true)
                                 price: Long,
                                 @ApiModelProperty(required = true, example = "1000000")
                                 amount: Long,
                                 @ApiModelProperty(required = true)
                                 fee: Long,
                                 @ApiModelProperty(required = true)
                                 buyMatcherFee: Long,
                                 @ApiModelProperty(required = true)
                                 sellMatcherFee: Long,
                                 @ApiModelProperty(required = true)
                                 timestamp: Long,
                                 @ApiModelProperty(required = true)
                                 signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, ExchangeTransaction] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _t         <- ExchangeTransaction.create(order1, order2, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, _signature)
    } yield _t
}
