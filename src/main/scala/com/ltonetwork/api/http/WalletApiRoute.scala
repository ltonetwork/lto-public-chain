package com.ltonetwork.api.http

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.ltonetwork.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json.Json
import com.ltonetwork.utils.Base58
import com.ltonetwork.wallet.Wallet

@Path("/wallet")
@Api(value = "/wallet")
case class WalletApiRoute(settings: RestAPISettings, wallet: Wallet) extends ApiRoute {

  override lazy val route = seed

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Export wallet seed", httpMethod = "GET")
  def seed: Route = (path("wallet" / "seed") & get & withAuth) {
    wallet.seed match {
      case Right(w) => complete(Json.obj("seed" -> Base58.encode(w)))
      case Left(l)  => complete(l)
    }
  }
}