package com.ltonetwork.api.http.requests.anchor

import play.api.libs.json.Json

case class AnchorV1Request(version: Byte, sender: String, anchors: List[String], fee: Long, timestamp: Option[Long] = None)

object AnchorV1Request {
  implicit val unsignedDataRequestReads = Json.reads[AnchorV1Request]
  implicit val signedDataRequestReads   = Json.reads[SignedAnchorV1Request]
}
