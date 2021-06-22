package com.ltonetwork.api.http.requests.transfer

import com.ltonetwork.transaction.transfer.MassTransferTransaction.Transfer
import play.api.libs.json.Json

case class MassTransferV1Request(version: Byte,
                                 sender: String,
                                 transfers: List[Transfer],
                                 fee: Long,
                                 attachment: Option[String],
                                 timestamp: Option[Long] = None)

object MassTransferV1Request {
  implicit val reads = Json.reads[MassTransferV1Request]
}
