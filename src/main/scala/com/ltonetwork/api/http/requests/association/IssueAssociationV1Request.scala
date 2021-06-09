package com.ltonetwork.api.http.requests.association

import play.api.libs.json.Json

case class IssueAssociationV1Request(version: Byte,
                                     sender: String,
                                     party: String,
                                     associationType: Int,
                                     hash: String = "",
                                     fee: Long,
                                     timestamp: Option[Long] = None)

object IssueAssociationV1Request {
  implicit val unsignedDataRequestReads = Json.reads[IssueAssociationV1Request]
  implicit val signedDataRequestReads   = Json.reads[SignedIssueAssociationV1Request]
}