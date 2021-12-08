package com.ltonetwork.consensus.nxt.api.http

import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.api.http.{ApiRoute, CommonApiFunctions, InvalidAddress}
import com.ltonetwork.consensus.GeneratingBalanceProvider
import com.ltonetwork.settings.{FunctionalitySettings, RestAPISettings}
import com.ltonetwork.state.Blockchain
import jakarta.ws.rs.{GET, Path}
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.Schema
import io.swagger.v3.oas.annotations.tags.Tag
import play.api.libs.json.Json

@Path("/consensus")
@Tag(name = "consensus")
case class NxtConsensusApiRoute(settings: RestAPISettings, blockchain: Blockchain, fs: FunctionalitySettings)
    extends ApiRoute
    with CommonApiFunctions {

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ basetarget ~ baseTargetId ~ generationSignature ~ generationSignatureId ~ generatingBalance
    }

  @GET
  @Path("/generatingbalance/{address}")
  @Operation(
    summary = "Account's generating balance (same as balance atm)"
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
  def generatingBalance: Route = (path("generatingbalance" / Segment) & get) { address =>
    Address.fromString(address) match {
      case Left(_) => complete(InvalidAddress)
      case Right(account) =>
        complete(Json.obj("address" -> account.address, "balance" -> GeneratingBalanceProvider.balance(blockchain, fs, account)))
    }
  }

  @GET
  @Path("/generationsignature/{blockId}")
  @Operation(
    summary = "Generation signature of a block with specified id"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "blockId",
        description = "Id of the block",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      )
    )
  )
  def generationSignatureId: Route = (path("generationsignature" / Segment) & get) { encodedSignature =>
    withBlock(blockchain, encodedSignature) { block =>
      complete(Json.obj("generationSignature" -> block.consensusData.generationSignature.base58))
    }
  }

  @GET
  @Path("/generationsignature")
  @Operation(
    summary = "Generation signature of the last block"
  )
  def generationSignature: Route = (path("generationsignature") & get) {
    complete(Json.obj("generationSignature" -> blockchain.lastBlock.get.consensusData.generationSignature.base58))
  }

  @GET
  @Path("/basetarget/{blockId}")
  @Operation(
    summary = "Base target of a block with specified id"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "blockId",
        description = "Id of the block",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      )
    )
  )
  def baseTargetId: Route = (path("basetarget" / Segment) & get) { encodedSignature =>
    withBlock(blockchain, encodedSignature) { block =>
      complete(Json.obj("baseTarget" -> block.consensusData.baseTarget))
    }
  }

  @GET
  @Path("/basetarget")
  @Operation(
    summary = "Base target of the last block"
  )
  def basetarget: Route = (path("basetarget") & get) {
    complete(
      Json.obj(
        "baseTarget" -> blockchain.lastBlock.get.consensusData.baseTarget,
        "score"      -> blockchain.score.toString()
      ))
  }

  @GET
  @Path("/algo")
  @Operation(
    summary = "Shows which consensus algorithm is being using"
  )
  def algo: Route = (path("algo") & get) {
    complete(Json.obj("consensusAlgo" -> "Fair Proof-of-Stake (FairPoS)"))
  }
}
