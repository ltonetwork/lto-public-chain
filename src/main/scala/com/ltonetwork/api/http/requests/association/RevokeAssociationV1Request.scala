package com.ltonetwork.api.http.requests.association

import play.api.libs.json.Json

case class RevokeAssociationV1Request(version: Byte,
                                      sender: String,
                                      party: String,
                                      associationType: Int,
                                      hash: String = "",
                                      fee: Long,
                                      timestamp: Option[Long] = None)

object RevokeAssociationV1Request {
  implicit val unsignedDataRequestReads = Json.reads[RevokeAssociationV1Request]
  implicit val signedDataRequestReads   = Json.reads[SignedRevokeAssociationV1Request]
}
