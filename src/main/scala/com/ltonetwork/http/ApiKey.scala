package com.ltonetwork.http

import akka.http.scaladsl.model.headers._

import scala.util.Try

object ApiKey extends ModeledCustomHeaderCompanion[ApiKey] {
  override val name                 = "X-API-Key"
  override def parse(value: String) = Try(new ApiKey(value))
}

final class ApiKey(override val value: String) extends ModeledCustomHeader[ApiKey] {
  override def companion         = ApiKey
  override def renderInRequests  = true
  override def renderInResponses = false
}
