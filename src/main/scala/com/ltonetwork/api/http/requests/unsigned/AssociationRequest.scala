package com.ltonetwork.api.http.requests.unsigned

import com.ltonetwork.api.http.requests.signed.SignedAssociationRequest
import play.api.libs.json.Json

case class AssociationRequest(version: Byte,
                              sender: String,
                              party: String,
                              associationType: Int,
                              hash: String = "",
                              fee: Long,
                              timestamp: Option[Long] = None)

object AssociationRequest {
  implicit val unsignedDataRequestReads = Json.reads[AssociationRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedAssociationRequest]
}