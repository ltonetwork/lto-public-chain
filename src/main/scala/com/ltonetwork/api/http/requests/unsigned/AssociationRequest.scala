package com.ltonetwork.api.http.requests.unsigned

case class AssociationRequest(version: Byte,
                              sender: String,
                              party: String,
                              associationType: Int,
                              hash: String = "",
                              fee: Long,
                              timestamp: Option[Long] = None)
