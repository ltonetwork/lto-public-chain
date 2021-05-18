package com.ltonetwork.api.http.requests.unsigned

case class SponsorshipRequest(version: Byte, sender: String, recipient: String, fee: Long, timestamp: Option[Long] = None)
