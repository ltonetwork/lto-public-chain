package com.ltonetwork.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.api.http.SponsorshipApiRoute.SponsorshipInfo
import com.ltonetwork.http.BroadcastRoute
import com.ltonetwork.settings.RestAPISettings
import com.ltonetwork.state.Blockchain
import com.ltonetwork.utils.Time
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.Schema
import io.swagger.v3.oas.annotations.tags.Tag
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import jakarta.ws.rs.{GET, Path}
import play.api.libs.json.{Format, Json}

@Path("/sponsorship")
@Tag(name = "sponsorship")
case class SponsorshipApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utx: UtxPool, allChannels: ChannelGroup, time: Time)
    extends ApiRoute
    with BroadcastRoute {

  override val route = pathPrefix("sponsorship") {
    status
  }

  @GET
  @Path("/status/{address}")
  @Operation(
    summary = "Get all active sponsorship for an address"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "address",
        description = "Wallet address",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      )
    )
  )
  def status: Route = (pathPrefix("status") & get) {
    pathPrefix(Segment) { address =>
      complete(Address.fromString(address) match {
        case Left(e) => ApiError.fromValidationError(e)
        case Right(a) =>
          ToResponseMarshallable(SponsorshipInfo(blockchain.sponsorOf(a).map(_.address)))
      })
    }
  }
}

object SponsorshipApiRoute {
  case class SponsorshipInfo(sponsor: List[String])
  implicit val sponsorshipInfoFormat: Format[SponsorshipInfo] = Json.format

}
