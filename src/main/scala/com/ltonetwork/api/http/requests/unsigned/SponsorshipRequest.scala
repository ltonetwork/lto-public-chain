package com.ltonetwork.api.http.requests.unsigned

import play.api.libs.json.{Format, Json}

case class SponsorshipRequest(version: Byte, sender: String, recipient: String, fee: Long, timestamp: Option[Long] = None)

object SponsorshipRequest {
  implicit val format: Format[SponsorshipRequest] = Json.format
}
