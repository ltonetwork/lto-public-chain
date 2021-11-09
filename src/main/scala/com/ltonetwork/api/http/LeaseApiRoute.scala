package com.ltonetwork.api.http

import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.http.BroadcastRoute
import com.ltonetwork.settings.RestAPISettings
import com.ltonetwork.state.Blockchain
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.utils.Time
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import jakarta.validation.Path
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.Schema
import io.swagger.v3.oas.annotations.tags.Tag
import play.api.libs.json.JsNumber

@Path("/leasing")
@Tag("leasing")
case class LeaseApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utx: UtxPool, allChannels: ChannelGroup, time: Time)
    extends ApiRoute
    with BroadcastRoute {

  override val route: Route = pathPrefix("leasing") {
    active
  }

  @Path("/active/{address}")
  @Operation(
    summary = "Get all active leases for an address",
    method = "GET"
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
  def active: Route = (pathPrefix("active") & get) {
    pathPrefix(Segment) { address =>
      complete(Address.fromString(address) match {
        case Left(e) => ApiError.fromValidationError(e)
        case Right(a) =>
          blockchain
            .addressTransactions(a, Set(LeaseTransaction.typeId), Int.MaxValue, 0)
            .collect {
              case (h, lt: LeaseTransaction) if blockchain.leaseDetails(lt.id()).exists(_.isActive) =>
                lt.json() + ("height" -> JsNumber(h))
            }
      })
    }
  }
}
