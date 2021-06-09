package com.ltonetwork.api.http.requests.sponsorship

import play.api.libs.json.{Format, Json}

case class CancelSponsorshipV1Request(version: Byte, sender: String, recipient: String, fee: Long, timestamp: Option[Long] = None)

object CancelSponsorshipV1Request {
  implicit val format: Format[CancelSponsorshipV1Request] = Json.format
}
