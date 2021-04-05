package com.ltonetwork.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.api.http.SponsorshipApiRoute.SponsorshipInfo
import com.ltonetwork.http.BroadcastRoute
import com.ltonetwork.settings.RestAPISettings
import com.ltonetwork.state.Blockchain
import com.ltonetwork.transaction._
import com.ltonetwork.utils.Time
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.{Format, JsNumber, Json}

@Path("/sponsorship")
@Api(value = "/sponsorship")
case class SponsorshipApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utx: UtxPool, allChannels: ChannelGroup, time: Time)
    extends ApiRoute
    with BroadcastRoute {

  override val route = pathPrefix("sponsorship") {
    sponsor ~ cancel ~ status
  }

  @Path("/sponsor")
  @ApiOperation(value = "Sponsor another account", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.ltonetwork.api.http.SponsorshipRequest",
        defaultValue =
          "{\n                       \"version\": 1,\n                       \"sender\": \"3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg\",\n                       \"recipient\" : \"3M8CzCtoURGBVQn9FXkhvz8vXwq2KHZbDmC\",\n                       \"fee\": 100000,\n                       }"
      )
    ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def sponsor: Route = processRequest("lease", (t: SponsorshipRequest) => doBroadcast(TransactionFactory.sponsorship(t, wallet, time)))

  @Path("/cancel")
  @ApiOperation(value = "Interrupt sponsorship", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.ltonetwork.api.http.SponsorshipRequest",
        defaultValue =
          "{\n                       \"version\": 1,\n                       \"sender\": \"3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg\",\n                       \"recipient\" : \"3M8CzCtoURGBVQn9FXkhvz8vXwq2KHZbDmC\",\n                       \"fee\": 100000,\n                       }"
      )
    ))
  def cancel: Route = processRequest("cancel", (t: SponsorshipRequest) => doBroadcast(TransactionFactory.cancelSponsorship(t, wallet, time)))

  @Path("/status/{address}")
  @ApiOperation(value = "Get all active sponsorship for an address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path")
    ))
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
