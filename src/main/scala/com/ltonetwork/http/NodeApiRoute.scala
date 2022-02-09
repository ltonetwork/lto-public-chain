package com.ltonetwork.http

import akka.http.scaladsl.server.Route
import com.ltonetwork.Shutdownable
import com.ltonetwork.api.{ApiRoute, CommonApiFunctions}
import com.ltonetwork.settings.{Constants, RestAPISettings}
import com.ltonetwork.state.Blockchain
import com.ltonetwork.utils.ScorexLogging
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.responses.ApiResponse
import io.swagger.v3.oas.annotations.tags.Tag
import jakarta.ws.rs.{GET, POST, Path}
import play.api.libs.json.Json

import java.time.Instant

@Path("/node")
@Tag(name = "node")
case class NodeApiRoute(settings: RestAPISettings, blockchain: Blockchain, application: Shutdownable)
    extends ApiRoute
    with CommonApiFunctions
    with ScorexLogging {

  override lazy val route = pathPrefix("node") {
    stop ~ status ~ version
  }

  @GET
  @Path("/version")
  @Operation(
    summary = "Get LTO node version",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Json LTO node version")
    )
  )
  def version: Route = (get & path("version")) {
    complete(Json.obj("version" -> Constants.AgentName))
  }

  @POST
  @Path("/stop")
  @Operation(
    summary = "Stop the node"
  )
  def stop: Route = (post & path("stop") & withAuth) {
    log.info("Request to stop application")
    application.shutdown()
    complete(Json.obj("stopped" -> true))
  }

  @GET
  @Path("/status")
  @Operation(
    summary = "Get status of the running core"
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
