package com.ltonetwork.api.http.requests.lease

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class CancelLeaseV1Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                sender: String,
                                @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                txId: String,
                                @ApiModelProperty(required = true)
                                fee: Long,
                                timestamp: Option[Long] = None)

object CancelLeaseV1Request {
  implicit val leaseCancelRequestFormat: Format[CancelLeaseV1Request] = Json.format
}
