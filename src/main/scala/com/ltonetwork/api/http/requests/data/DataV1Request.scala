package com.ltonetwork.api.http.requests.data

import com.ltonetwork.state.DataEntry
import play.api.libs.json.Json

case class DataV1Request(version: Byte, sender: String, data: List[DataEntry[_]], fee: Long, timestamp: Option[Long] = None)

object DataV1Request {
  implicit val reads = Json.reads[DataV1Request]
}