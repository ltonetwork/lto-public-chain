package com.ltonetwork.api.http.requests.unsigned

import com.ltonetwork.transaction.transfer.MassTransferTransaction.Transfer
import play.api.libs.json.Json

case class MassTransferRequest(version: Byte,
                               sender: String,
                               transfers: List[Transfer],
                               fee: Long,
                               attachment: Option[String],
                               timestamp: Option[Long] = None)

object MassTransferRequest {
  implicit val reads = Json.reads[MassTransferRequest]
}