package com.ltonetwork.features.api

import akka.http.scaladsl.server.Route
import com.ltonetwork.features.FeatureProvider._
import com.ltonetwork.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.ltonetwork.settings.{FeaturesSettings, FunctionalitySettings, RestAPISettings}
import com.ltonetwork.state.Blockchain
import jakarta.ws.rs.Path
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.tags.Tag
import play.api.libs.json.Json
import com.ltonetwork.api.http.{ApiRoute, CommonApiFunctions}
import com.ltonetwork.utils.ScorexLogging
import io.swagger.v3.oas.annotations.responses.{ApiResponse, ApiResponses}

@Path("/activation")
@Tag(name="activation")
case class ActivationApiRoute(settings: RestAPISettings,
                              functionalitySettings: FunctionalitySettings,
                              featuresSettings: FeaturesSettings,
                              blockchain: Blockchain)
    extends ApiRoute
    with CommonApiFunctions
    with ScorexLogging {

  override lazy val route: Route = pathPrefix("activation") {
    status
  }

  @Path("/status")
  @Operation(
    summary = "Get activation status",
    method = "GET"
  )
  @ApiResponses(
    Array(
      new ApiResponse(
        responseCode = "200",
        description = "Json activation status"
      )
    ))
  def status: Route = (get & path("status")) {

    val height = blockchain.height

    complete(
      Json.toJson(ActivationStatus(
        height,
        functionalitySettings.activationWindowSize(height),
        functionalitySettings.blocksForFeatureActivation(height),
        functionalitySettings.activationWindow(height).last,
        (blockchain.featureVotes(height).keySet ++
          blockchain.approvedFeatures.keySet ++
          BlockchainFeatures.implemented).toSeq.sorted.flatMap(id => {
          val status = blockchain.featureStatus(id, height)
          BlockchainFeatures.feature(id).map {
            case existing =>
              FeatureActivationStatus(
                id,
                existing.description,
                status,
                (BlockchainFeatures.implemented.contains(id), featuresSettings.supported.contains(id)) match {
                  case (false, _) => NodeFeatureStatus.NotImplemented
                  case (_, true)  => NodeFeatureStatus.Voted
                  case _          => NodeFeatureStatus.Implemented
                },
                blockchain.featureActivationHeight(id),
                if (status == BlockchainFeatureStatus.Undefined) blockchain.featureVotes(height).get(id).orElse(Some(0)) else None
              )
          }
        })
      )))
  }
}
