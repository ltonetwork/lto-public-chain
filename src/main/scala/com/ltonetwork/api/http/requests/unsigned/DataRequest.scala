package com.ltonetwork.api.http.requests.unsigned

import com.ltonetwork.api.http.requests.signed.SignedDataRequest
import com.ltonetwork.state.DataEntry
import play.api.libs.json.Json

case class DataRequest(version: Byte, sender: String, data: List[DataEntry[_]], fee: Long, timestamp: Option[Long] = None)

object DataRequest {
  implicit val unsignedDataRequestReads = Json.reads[DataRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedDataRequest]
}