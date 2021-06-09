package com.ltonetwork.api.http.requests.sponsorship

import play.api.libs.json.{Format, Json}

case class SponsorshipV1Request(version: Byte, sender: String, recipient: String, fee: Long, timestamp: Option[Long] = None)

object SponsorshipV1Request {
  implicit val format: Format[SponsorshipV1Request] = Json.format
}
