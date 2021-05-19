package com.ltonetwork.api.http.requests.unsigned

import com.ltonetwork.api.http.requests.signed.SignedAnchorRequest
import play.api.libs.json.Json

case class AnchorRequest(version: Byte, sender: String, anchors: List[String], fee: Long, timestamp: Option[Long] = None)

object AnchorRequest {
  implicit val unsignedDataRequestReads = Json.reads[AnchorRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedAnchorRequest]
}
