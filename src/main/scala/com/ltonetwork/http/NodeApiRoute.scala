package com.ltonetwork.http

import java.time.Instant
import akka.http.scaladsl.server.Route
import com.ltonetwork.Shutdownable
import com.ltonetwork.settings.{Constants, RestAPISettings}
import com.ltonetwork.state.Blockchain
import jakarta.ws.rs.Path
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.responses.ApiResponse
import io.swagger.v3.oas.annotations.tags.Tag
import play.api.libs.json.Json
import com.ltonetwork.api.http.{ApiRoute, CommonApiFunctions}
import com.ltonetwork.utils.ScorexLogging

@Path("/node")
@Tag(name="node")
case class NodeApiRoute(settings: RestAPISettings, blockchain: Blockchain, application: Shutdownable)
    extends ApiRoute
    with CommonApiFunctions
    with ScorexLogging {

  override lazy val route = pathPrefix("node") {
    stop ~ status ~ version
  }

  @Path("/version")
  @Operation(
    summary = "Get LTO node version",
    method = "GET",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Json LTO node version")
    )
  )
  def version: Route = (get & path("version")) {
    complete(Json.obj("version" -> Constants.AgentName))
  }

  @Path("/stop")
  @Operation(
    summary = "Stop the node",
    method = "POST"
  )
  def stop: Route = (post & path("stop") & withAuth) {
    log.info("Request to stop application")
    application.shutdown()
    complete(Json.obj("stopped" -> true))
  }

  @Path("/status")
  @Operation(
    summary = "Get status of the running core",
    method = "GET"
  )
  def status: Route = (get & path("status")) {
    val lastUpdated = blockchain.lastBlock.get.timestamp
    complete(
      Json.obj(
        "blockchainHeight" -> blockchain.height,
        "stateHeight"      -> blockchain.height,
        "updatedTimestamp" -> lastUpdated,
        "updatedDate"      -> Instant.ofEpochMilli(lastUpdated).toString
      ))
  }
}
