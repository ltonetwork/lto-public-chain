package com.wavesplatform.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Address
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{Blockchain, ByteStr}
import com.wavesplatform.transaction.AssociationTransaction.ActionType.{Issue, Revoke}
import com.wavesplatform.transaction.AssociationTransaction.Assoc
import com.wavesplatform.transaction.{AssociationTransactionBase, TransactionFactory}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json._

@Path("/associations")
@Api(value = "/associations/")
case class AssociationsApiRoute(settings: RestAPISettings,
                                wallet: Wallet,
                                blockchain: Blockchain,
                                utx: UtxPool,
                                allChannels: ChannelGroup,
                                time: Time)
    extends ApiRoute
    with BroadcastRoute {

  import AssociationsApiRoute._

  override lazy val route =
    pathPrefix("associations") {
      issueAssociation ~ revokeAssociation ~ associations
    }

  @Path("/status/{address}")
  @ApiOperation(value = "Details for account", notes = "Account's associations", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def associations: Route = (path("status" / Segment) & get) { address =>
    complete(
      Address
        .fromString(address)
        .right
        .map(acc => {
          ToResponseMarshallable(associationsJson(acc, blockchain.associations(acc)))
        })
        .getOrElse(InvalidAddress)
    )
  }

  private def associationsJson(address: Address, a: Blockchain.Associations): AssociationsInfo = {
    def f(l: List[(Int, AssociationTransactionBase)]) = {
      l.foldLeft(Map.empty[Assoc, (Int, Address, ByteStr, Option[(Int, ByteStr)])]) {
          case (acc, (height, as: AssociationTransactionBase)) =>
            val cp = if (address == as.sender.toAddress) as.assoc.party else as.sender.toAddress
            (as.actionType, acc.get(as.assoc)) match {
              case (Issue, None)                    => acc + (as.assoc -> (height, cp, as.id(), None))
              case (Revoke, Some((h, _, bs, None))) => acc + (as.assoc -> (h, cp, bs, Some((height, as.id()))))
              case _                                => acc
            }
        }
        .toList
        .sortBy(_._2._1)
        .map {
          case (assoc, (h, cp, id, r)) =>
            AssociationInfo(
              party = cp.stringRepr,
              hash = assoc.hashStr,
              associationType = assoc.assocType,
              issueHeight = h,
              issueTransactionId = id.toString,
              revokeHeight = r.map(_._1),
              revokeTransactionId = r.map(_._2.toString)
            )
        }
    }

    AssociationsInfo(address.stringRepr, f(a.outgoing), f(a.incoming))

  }

  @Path("/issue")
  @ApiOperation(value = "Creates an association between accounts", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.api.http.AssociationRequest",
        defaultValue =
          "\n   {\n       \"sender\": \"3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg\",\n       \"fee\": 100000,\n       \"version\": 1,\n       \"party\" : \"3MSDfsdfsdfsdfsdfsdfsdsdfsdfsdfsdf\",\n       \"associationType\" : 420,\n       \"hash\" : \"\"\n   }\n"
      )
    ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def issueAssociation: Route =
    processRequest(
      "issue",
      (req: AssociationRequest) => {
        doBroadcast(
          TransactionFactory
            .issueAssociation(req, wallet, time)
            .flatMap(TransactionsApiRoute.ifPossible(blockchain, _)))
      }
    )

  @Path("/revoke")
  @ApiOperation(value = "Revokes an association between accounts", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.api.http.AssociationRequest",
        defaultValue =
          "\n   {\n       \"sender\": \"3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg\",\n       \"fee\": 100000,\n       \"version\": 1,\n       \"party\" : \"3MSDfsdfsdfsdfsdfsdfsdsdfsdfsdfsdf\",\n       \"associationType\" : 420,\n       \"hash\" : \"\"\n   }\n"
      )
    ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def revokeAssociation: Route =
    processRequest(
      "revoke",
      (req: AssociationRequest) =>
        doBroadcast(
          TransactionFactory
            .revokeAssociation(req, wallet, time)
            .flatMap(TransactionsApiRoute.ifPossible(blockchain, _)))
    )

}

object AssociationsApiRoute {

  case class AssociationInfo(party: String,
                             hash: String,
                             associationType: Int,
                             issueHeight: Int,
                             issueTransactionId: String,
                             revokeHeight: Option[Int],
                             revokeTransactionId: Option[String])

  case class AssociationsInfo(address: String, outgoing: List[AssociationInfo], incoming: List[AssociationInfo])

  implicit val associactionInfoFormat: Format[AssociationInfo]   = Json.format
  implicit val associactionsInfoFormat: Format[AssociationsInfo] = Json.format

}
