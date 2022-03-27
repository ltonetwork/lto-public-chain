package com.ltonetwork.api

import akka.http.scaladsl.server.Route
import com.ltonetwork.settings.{GenesisSettings, RestAPISettings}
import com.ltonetwork.state.Blockchain
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import io.swagger.v3.oas.annotations.media.Schema
import io.swagger.v3.oas.annotations.tags.Tag
import jakarta.ws.rs.{GET, Path}
import play.api.libs.json.{Format, Json}

@Path("/supply")
@Tag(name = "supply")
case class SupplyApiRoute(settings: RestAPISettings,
                          blockchain: Blockchain,
                          genesisSettings: GenesisSettings,
                         ) extends ApiRoute {
  import SupplyApiRoute._

  override lazy val route = pathPrefix("supply") { at } ~ root

  @GET
  @Path("/")
  @Operation(
    summary = "Get information about total LTO supply",
  )
  def root: Route = (path("supply") & get) {
    complete(Supply(
      height = blockchain.height,
      initial = genesisSettings.initialBalance,
      burned = blockchain.burned,
      total = genesisSettings.initialBalance - blockchain.burned,
    ))
  }

  @GET
  @Path("/at/{height}")
  @Operation(
    summary = "Get supply at specified height"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "height",
        description = "Block height",
        required = true,
        schema = new Schema(implementation = classOf[Int]),
        in = ParameterIn.PATH
      )
    )
  )
  def at: Route = (path("at" / IntNumber) & get) { height =>
    Either.cond(height < blockchain.height, blockchain.burned(height), BlockDoesNotExist).map(burned => Supply(
      height = height,
      initial = genesisSettings.initialBalance,
      burned = burned,
      total = genesisSettings.initialBalance - burned,
    )) match {
      case Right(s) => complete(s)
      case Left(e)  => complete(e)
    }
  }
}

object SupplyApiRoute {
  case class Supply(height: Int, initial: Long, burned: Long, total: Long)

  implicit val supplyFormat: Format[Supply] = Json.format
}