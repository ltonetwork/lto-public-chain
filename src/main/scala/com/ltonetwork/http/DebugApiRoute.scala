package com.ltonetwork.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.api._
import com.ltonetwork.block.Block
import com.ltonetwork.block.Block.BlockId
import com.ltonetwork.crypto
import com.ltonetwork.mining.{Miner, MinerDebugInfo}
import com.ltonetwork.network.{LocalScoreChanged, PeerDatabase, PeerInfo, _}
import com.ltonetwork.settings.LtoSettings
import com.ltonetwork.state.{ByteStr, LeaseBalance, NG, Portfolio}
import com.ltonetwork.transaction._
import com.ltonetwork.utils.{Base58, ScorexLogging}
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import com.typesafe.config.{ConfigObject, ConfigRenderOptions}
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.responses.{ApiResponse, ApiResponses}
import io.swagger.v3.oas.annotations.security.SecurityRequirement
import io.swagger.v3.oas.annotations.tags.Tag
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import jakarta.ws.rs.{DELETE, GET, POST, Path}
import monix.eval.{Coeval, Task}
import play.api.libs.json._

import java.net.{InetAddress, InetSocketAddress, URI}
import java.util.concurrent.ConcurrentMap
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

@Path("/debug")
@Tag(name = "debug")
case class DebugApiRoute(ws: LtoSettings,
                         wallet: Wallet,
                         ng: NG,
                         peerDatabase: PeerDatabase,
                         establishedConnections: ConcurrentMap[Channel, PeerInfo],
                         rollbackTask: ByteStr => Task[Either[ValidationError, Seq[Block]]],
                         allChannels: ChannelGroup,
                         utxStorage: UtxPool,
                         miner: Miner with MinerDebugInfo,
                         historyReplier: HistoryReplier,
                         extLoaderStateReporter: Coeval[RxExtensionLoader.State],
                         mbsCacheSizesReporter: Coeval[MicroBlockSynchronizer.CacheSizes],
                         scoreReporter: Coeval[RxScoreObserver.Stats],
                         configRoot: ConfigObject)
    extends ApiRoute
    with ScorexLogging {

  import DebugApiRoute._

  private lazy val configStr           = configRoot.render(ConfigRenderOptions.concise().setJson(true).setFormatted(true))
  private lazy val fullConfig: JsValue = Json.parse(configStr)
  private lazy val ltoConfig: JsObject = Json.obj("lto" -> (fullConfig \ "lto").get)

  override val settings = ws.restAPISettings
  override lazy val route: Route = pathPrefix("debug") {
    blocks ~ state ~ info ~ stateAtHeight ~ rollback ~ rollbackTo ~ blacklist ~ portfolios ~ minerInfo ~ historyInfo ~ configInfo ~ print
  }

  @GET
  @Path("/blocks/{howMany}")
  @Operation(
    summary = "Get sizes and full hashes for last blocks",
  )
  @SecurityRequirement(name = "bearerAuth")
  @Parameters(
    Array(
      new Parameter(
        name = "howMany",
        description = "How many last blocks to take",
        required = true,
        schema = new Schema(implementation = classOf[Int]),
        in = ParameterIn.PATH
      )
    )
  )
  def blocks: Route = {
    (path("blocks" / IntNumber) & get & withAuth) { howMany =>
      complete(JsArray(ng.lastBlocks(howMany).map { block =>
        val bytes = block.bytes()
        Json.obj(bytes.length.toString -> Base58.encode(crypto.fastHash(bytes)))
      }))
    }
  }

  @POST
  @Path("/print")
  @Operation(
    summary = "Prints a string at DEBUG level, strips to 100 chars"
  )
  @SecurityRequirement(name = "bearerAuth")
  @RequestBody(
    description = "Json with data",
    content = Array(
      new Content(
        schema = new Schema(implementation = classOf[com.ltonetwork.http.DebugMessage]),
        examples = Array(
          new ExampleObject(
            value = "{\n\t\"message\": \"foo\"\n}",
          ))
      )),
    required = true
  )
  @ApiResponses(Array(new ApiResponse(responseCode = "200", description = "Json portfolio")))
  def print: Route = (path("print") & post & withAuth) {
    json[DebugMessage] { params =>
      log.debug(params.message.take(250))
      ""
    }
  }

  @GET
  @Path("/portfolios/{address}")
  @Operation(
    summary = "Get current portfolio considering pessimistic transactions in the UTX pool"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "address",
        description = "An address of portfolio",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      ),
      new Parameter(
        name = "considerUnspent",
        description = "Taking into account pessimistic transactions from UTX pool",
        required = false,
        schema = new Schema(implementation = classOf[Boolean]),
        in = ParameterIn.QUERY
      )
    )
  )
  @ApiResponses(Array(new ApiResponse(responseCode = "200", description = "Json portfolio")))
  def portfolios: Route = path("portfolios" / Segment) { rawAddress =>
    (get & withAuth & parameter('considerUnspent.as[Boolean])) { considerUnspent =>
      Address.fromString(rawAddress) match {
        case Left(_) => complete(InvalidAddress)
        case Right(address) =>
          val portfolio = if (considerUnspent) utxStorage.portfolio(address) else ng.portfolio(address)
          complete(Json.toJson(portfolio))
      }
    }
  }

  @GET
  @Path("/state")
  @Operation(
    summary = "Get current state"
  )
  @SecurityRequirement(name = "bearerAuth")
  @ApiResponses(Array(new ApiResponse(responseCode = "200", description = "Json state")))
  def state: Route = (path("state") & get & withAuth) {
    complete(ng.ltoDistribution(ng.height).map { case (a, b) => a.stringRepr -> b })
  }

  @GET
  @Path("/stateAtHeight/{height}")
  @Operation(
    summary = "Get state at specified height"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "height",
        description = "Height at which the state should be retrieved",
        required = true,
        schema = new Schema(implementation = classOf[Int]),
        in = ParameterIn.PATH
      )
    )
  )
  def stateAtHeight: Route = (path("stateAtHeight" / IntNumber) & get & withAuth) { height =>
    complete(ng.ltoDistribution(height).map { case (a, b) => a.stringRepr -> b })
  }

  private def rollbackToBlock(blockId: ByteStr, returnTransactionsToUtx: Boolean): Future[ToResponseMarshallable] = {
    import monix.execution.Scheduler.Implicits.global

    rollbackTask(blockId).asyncBoundary.map {
      case Right(blocks) =>
        allChannels.broadcast(LocalScoreChanged(ng.score))
        if (returnTransactionsToUtx) {
          utxStorage.batched { ops =>
            blocks.flatMap(_.transactionData).foreach(ops.putIfNew)
          }
        }
        miner.scheduleMining()
        Json.obj("BlockId" -> blockId.toString): ToResponseMarshallable
      case Left(error) => ApiError.fromValidationError(error): ToResponseMarshallable
    }.runAsyncLogErr
  }

  @POST
  @Path("/rollback")
  @Operation(
    summary = "Removes all blocks after given height"
  )
  @SecurityRequirement(name = "bearerAuth")
  @RequestBody(
    description = "Json with data",
    content = Array(
      new Content(
        schema = new Schema(implementation = classOf[com.ltonetwork.http.RollbackParams]),
        examples = Array(
          new ExampleObject(
            value = "{\n\t\"rollbackTo\": 3,\n\t\"returnTransactionsToUTX\": false\n}",
          ))
      )),
    required = true
  )
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "200 if success, 404 if there are no block at this height")
    ))
  def rollback: Route = (path("rollback") & post & withAuth) {
    json[RollbackParams] { params =>
      ng.blockAt(params.rollbackTo) match {
        case Some(block) =>
          rollbackToBlock(block.uniqueId, params.returnTransactionsToUtx)
        case None =>
          (StatusCodes.BadRequest, "Block at height not found")
      }
    } ~ complete(StatusCodes.BadRequest)
  }

  @GET
  @Path("/info")
  @Operation(
    summary = "All info you need to debug"
  )
  @SecurityRequirement(name = "bearerAuth")
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "Json state")
    ))
  def info: Route = (path("info") & get & withAuth) {
    complete(
      Json.obj(
        "stateHeight"                      -> ng.height,
        "extensionLoaderState"             -> extLoaderStateReporter().toString,
        "historyReplierCacheSizes"         -> Json.toJson(historyReplier.cacheSizes),
        "microBlockSynchronizerCacheSizes" -> Json.toJson(mbsCacheSizesReporter()),
        "scoreObserverStats"               -> Json.toJson(scoreReporter()),
        "minerState"                       -> Json.toJson(miner.state)
      ))
  }

  @GET
  @Path("/minerInfo")
  @Operation(
    summary = "All miner info you need to debug"
  )
  @SecurityRequirement(name = "bearerAuth")
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "Json state")
    ))
  def minerInfo: Route = (path("minerInfo") & get & withAuth) {
    complete(miner.collectNextBlockGenerationTimes.map {
      case (a, t) =>
        AccountMiningInfo(
          address = a.stringRepr,
          miningBalance = ng.effectiveBalance(a,
                                              ws.blockchainSettings.functionalitySettings.generatingBalanceDepth(ng.height),
                                              ng.microblockIds.lastOption.getOrElse(ByteStr.empty)),
          timestamp = t
        )
    })
  }

  @GET
  @Path("/historyInfo")
  @Operation(
    summary = "All history info you need to debug"
  )
  @SecurityRequirement(name = "bearerAuth")
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "Json state")
    ))
  def historyInfo: Route = (path("historyInfo") & get & withAuth) {
    val a = ng.lastPersistedBlockIds(10)
    val b = ng.microblockIds
    complete(HistoryInfo(a, b))

  }

  @GET
  @Path("/configInfo")
  @Operation(
    summary = "Currently running node config"
  )
  @SecurityRequirement(name = "bearerAuth")
  @Parameters(
    Array(
      new Parameter(
        name = "full",
        description = "Exposes full typesafe config",
        required = false,
        schema = new Schema(implementation = classOf[Boolean]),
        in = ParameterIn.QUERY
      )
    )
  )
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "Json state")
    ))
  def configInfo: Route = (path("configInfo") & get & parameter('full.as[Boolean]) & withAuth) { full =>
    complete(if (full) fullConfig else ltoConfig)
  }

  @DELETE
  @Path("/rollback-to/{signature}")
  @Operation(
    summary = "Rollback the state to the block with a given signature"
  )
  @SecurityRequirement(name = "bearerAuth")
  @Parameters(
    Array(
      new Parameter(
        name = "signature",
        description = "Base58-encoded block signature",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      )
    )
  )
  def rollbackTo: Route = path("rollback-to" / Segment) { signature =>
    (delete & withAuth) {
      ByteStr.decodeBase58(signature) match {
        case Success(sig) =>
          complete(rollbackToBlock(sig, returnTransactionsToUtx = false))
        case _ =>
          complete(InvalidSignature)
      }
    }
  }

  @POST
  @Path("/blacklist")
  @Operation(
    summary = "Moving peer to blacklist"
  )
  @SecurityRequirement(name = "bearerAuth")
  @RequestBody(
    description = "IP address of node",
    content = Array(
      new Content(
        schema = new Schema(implementation = classOf[String]),
      )),
    required = true
  )
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "200 if success, 404 if there are no peer with such address")
    ))
  def blacklist: Route = (path("blacklist") & post & withAuth) {
    entity(as[String]) { socketAddressString =>
      try {
        val uri     = new URI("node://" + socketAddressString)
        val address = InetAddress.getByName(uri.getHost)
        establishedConnections.entrySet().stream().forEach { entry =>
          entry.getValue.remoteAddress match {
            case x: InetSocketAddress if x.getAddress == address =>
              peerDatabase.blacklistAndClose(entry.getKey, "Debug API request")
            case _ =>
          }
        }
        complete(StatusCodes.OK)
      } catch {
        case NonFatal(_) => complete(StatusCodes.BadRequest)
      }
    } ~ complete(StatusCodes.BadRequest)
  }
}

object DebugApiRoute {
  implicit val assetsFormat: Format[Map[ByteStr, Long]] = Format[Map[ByteStr, Long]](
    {
      case JsObject(m) =>
        m.foldLeft[JsResult[Map[ByteStr, Long]]](JsSuccess(Map.empty)) {
          case (e: JsError, _) => e
          case (JsSuccess(m, _), (rawAssetId, JsNumber(count))) =>
            (ByteStr.decodeBase58(rawAssetId), count) match {
              case (Success(assetId), count) if count.isValidLong => JsSuccess(m.updated(assetId, count.toLong))
              case (Failure(_), _)                                => JsError(s"Can't parse '$rawAssetId' as base58 string")
              case (_, count)                                     => JsError(s"Invalid count of assets: $count")
            }
          case (_, (_, rawCount)) =>
            JsError(s"Invalid count of assets: $rawCount")
        }
      case _ => JsError("The map is expected")
    },
    m => Json.toJson(m.map { case (assetId, count) => assetId.base58 -> count })
  )
  implicit val leaseInfoFormat: Format[LeaseBalance] = Json.format
  implicit val portfolioFormat: Format[Portfolio]    = Json.format

  case class AccountMiningInfo(address: String, miningBalance: Long, timestamp: Long)

  implicit val accountMiningBalanceFormat: Format[AccountMiningInfo] = Json.format

  implicit val addressWrites: Format[Address] = new Format[Address] {
    override def writes(o: Address): JsValue = JsString(o.stringRepr)

    override def reads(json: JsValue): JsResult[Address] = ???
  }

  case class HistoryInfo(lastBlockIds: Seq[BlockId], microBlockIds: Seq[BlockId])

  implicit val historyInfoFormat: Format[HistoryInfo] = Json.format

  implicit val hrCacheSizesFormat: Format[HistoryReplier.CacheSizes]          = Json.format
  implicit val mbsCacheSizesFormat: Format[MicroBlockSynchronizer.CacheSizes] = Json.format
  implicit val BigIntWrite: Writes[BigInt]                                    = (bigInt: BigInt) => JsNumber(BigDecimal(bigInt))
  implicit val scoreReporterStatsWrite: Writes[RxScoreObserver.Stats]         = Json.writes[RxScoreObserver.Stats]

  import MinerDebugInfo._
  implicit val minerStateWrites: Writes[MinerDebugInfo.State] = (s: MinerDebugInfo.State) =>
    JsString(s match {
      case MiningBlocks      => "mining blocks"
      case MiningMicroblocks => "mining microblocks"
      case Disabled          => "disabled"
      case Error(err)        => s"error: $err"
    })
}
