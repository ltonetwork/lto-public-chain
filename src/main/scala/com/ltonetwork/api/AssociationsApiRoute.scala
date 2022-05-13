package com.ltonetwork.api

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.http.BroadcastRoute
import com.ltonetwork.settings.RestAPISettings
import com.ltonetwork.state.{Associations, Blockchain, ByteStr}
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.utils.Time
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.Schema
import io.swagger.v3.oas.annotations.tags.Tag
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import jakarta.ws.rs.{GET, Path}
import play.api.libs.json._

@Path("/associations")
@Tag(name = "associations")
case class AssociationsApiRoute(settings: RestAPISettings,
                                wallet: Wallet,
                                blockchain: Blockchain,
                                utx: UtxPool,
                                allChannels: ChannelGroup,
                                time: Time)
    extends ApiRoute
    with BroadcastRoute {

  override lazy val route: Route =
    pathPrefix("associations") {
      associations
    }

  @GET
  @Path("/status/{address}")
  @Operation(
    summary = "Account's associations"
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
  def associations: Route = (path("status" / Segment) & get) { address =>
    complete(
      Address
        .fromString(address)
        .right
        .map(address => {
          ToResponseMarshallable(blockchain.associations(address))
        })
        .getOrElse(InvalidAddress)
    )
  }
}
