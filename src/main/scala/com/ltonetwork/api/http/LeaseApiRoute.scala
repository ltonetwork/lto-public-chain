package com.ltonetwork.api.http

import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.api.http.requests.unsigned.{LeaseCancelV1Request, LeaseV1Request}
import com.ltonetwork.http.BroadcastRoute
import com.ltonetwork.settings.RestAPISettings
import com.ltonetwork.state.Blockchain
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.lease.{LeaseTransaction, LeaseTransactionV1}
import com.ltonetwork.utils.Time
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import play.api.libs.json.JsNumber

import javax.ws.rs.Path

@Path("/leasing")
@Api(value = "/leasing")
case class LeaseApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utx: UtxPool, allChannels: ChannelGroup, time: Time)
    extends ApiRoute
    with BroadcastRoute {

  override val route = pathPrefix("leasing") {
    active
  }

  @Path("/active/{address}")
  @ApiOperation(value = "Get all active leases for an address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path")
    ))
  def active: Route = (pathPrefix("active") & get) {
    pathPrefix(Segment) { address =>
      complete(Address.fromString(address) match {
        case Left(e) => ApiError.fromValidationError(e)
        case Right(a) =>
          blockchain
            .addressTransactions(a, Set(LeaseTransactionV1.typeId), Int.MaxValue, 0)
            .collect {
              case (h, lt: LeaseTransaction) if blockchain.leaseDetails(lt.id()).exists(_.isActive) =>
                lt.json() + ("height" -> JsNumber(h))
            }
      })
    }
  }
}
