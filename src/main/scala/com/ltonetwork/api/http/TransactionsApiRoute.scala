package com.ltonetwork.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import com.ltonetwork.account.Address
import com.ltonetwork.api.http.requests.TxRequest
import com.ltonetwork.http.BroadcastRoute
import com.ltonetwork.settings.{FeesSettings, FunctionalitySettings, RestAPISettings}
import com.ltonetwork.state.diffs.CommonValidation
import com.ltonetwork.state.{Blockchain, ByteStr}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.association.{IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.lease._
import com.ltonetwork.utils._
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import play.api.libs.json._
import javax.ws.rs.Path
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.tags.Tag

import java.util.NoSuchElementException
import scala.util.Success
import scala.util.control.Exception

@Path("/transactions")
@Tag(name="transactions")
case class TransactionsApiRoute(settings: RestAPISettings,
                                functionalitySettings: FunctionalitySettings,
                                feesSettings: FeesSettings,
                                wallet: Wallet,
                                blockchain: Blockchain,
                                utx: UtxPool,
                                allChannels: ChannelGroup,
                                time: Time)
    extends ApiRoute
    with BroadcastRoute
    with CommonApiFunctions {

  import TransactionsApiRoute.MaxTransactionsPerRequest

  override lazy val route: Route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info ~ sign ~ calculateFee ~ broadcast
    }

  private val invalidLimit = StatusCodes.BadRequest -> Json.obj("message" -> "invalid.limit")

  //TODO implement general pagination
  @Path("/address/{address}/limit/{limit}")
  @Operation(
    summary = "Get list of transactions where specified address has been involved",
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
      ),
      new Parameter(
        name = "limit",
        description = "Specified number of records to be returned",
        required = true,
        schema = new Schema(implementation = classOf[Int]),
        in = ParameterIn.PATH
      )
    )
  )
  def addressLimit: Route = (pathPrefix("address") & get) {
    pathPrefix(Segment) { address =>
      Address.fromString(address) match {
        case Left(e) => complete(ApiError.fromValidationError(e))
        case Right(a) =>
          pathPrefix("limit") {
            pathEndOrSingleSlash {
              complete(invalidLimit)
            } ~
              path(Segment) { limitStr =>
                Exception.allCatch.opt(limitStr.toInt) match {
                  case Some(limit) if limit > 0 && limit <= MaxTransactionsPerRequest =>
                    complete(
                      Json.arr(JsArray(blockchain
                        .addressTransactions(a, Set.empty, limit, 0)
                        .map({ case (h, tx) => txToCompactJson(a, tx) + ("height" -> JsNumber(h)) }))))
                  case Some(limit) if limit > MaxTransactionsPerRequest =>
                    complete(TooBigArrayAllocation)
                  case _ =>
                    complete(invalidLimit)
                }
              }
          } ~ complete(StatusCodes.NotFound)
      }
    }
  }

  @Path("/info/{id}")
  @Operation(
    summary = "Get transaction info",
    method = "GET"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "id",
        description = "Transaction id",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      )
    )
  )
  def info: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(Segment) { encoded =>
        ByteStr.decodeBase58(encoded) match {
          case Success(id) =>
            blockchain.transactionInfo(id) match {
              case Some((h, tx)) => complete(txToExtendedJson(tx) + ("height" -> JsNumber(h)))
              case None          => complete(StatusCodes.NotFound             -> Json.obj("status" -> "error", "details" -> "Transaction is not in blockchain"))
            }
          case _ => complete(InvalidSignature)
        }
      }
  }

  @Path("/unconfirmed")
  @Operation(
    summary = "Get list of unconfirmed transactions",
    method = "GET"
  )
  def unconfirmed: Route = (pathPrefix("unconfirmed") & get) {
    pathEndOrSingleSlash {
      complete(JsArray(utx.all.map(txToExtendedJson)))
    } ~ utxSize ~ utxTransactionInfo
  }

  @Path("/unconfirmed/size")
  @Operation(
    summary = "Get number of unconfirmed transactions in the UTX pool",
    method = "GET"
  )
  def utxSize: Route = (pathPrefix("size") & get) {
    complete(Json.obj("size" -> JsNumber(utx.size)))
  }

  @Path("/unconfirmed/info/{id}")
  @Operation(
    summary = "Get transaction that is in the UTX",
    method = "GET"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "id",
        description = "Transaction id",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      )
    )
  )
  def utxTransactionInfo: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(Segment) { encoded =>
        ByteStr.decodeBase58(encoded) match {
          case Success(id) =>
            utx.transactionById(id) match {
              case Some(tx) =>
                complete(txToExtendedJson(tx))
              case None =>
                complete(StatusCodes.NotFound -> Json.obj("status" -> "error", "details" -> "Transaction is not in UTX"))
            }
          case _ => complete(InvalidSignature)
        }
      }
  }

  @Path("/calculateFee")
  @Operation(
    summary = "Calculates a fee for a transaction",
    method = "POST"
  )
  @RequestBody(
    description = "Transaction data including type and optional timestamp in milliseconds",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[String]),
    )),
    required = true
  )
  def calculateFee: Route = (pathPrefix("calculateFee") & post) {
    pathEndOrSingleSlash {
      handleExceptions(jsonExceptionHandler) {
        json[JsObject] { jsv =>
          val enoughFee = 1000.lto
          // Just for converting the request to the transaction
          val enrichedJsv = jsv ++ Json.obj(
            "fee" -> enoughFee,
          )
          createTransaction(enrichedJsv) { tx =>
            for {
              commonMinFee <- CommonValidation.getMinFee(blockchain, functionalitySettings, blockchain.height, tx)
              utxMinFee    <- new FeeCalculator(feesSettings, blockchain).minFee(tx)
              minFee = Math.max(commonMinFee, utxMinFee)
            } yield Json.obj("feeAmount" -> minFee)
          }
        }
      }
    }
  }

  @Path("/sign")
  @Operation(
    summary = "Sign a transaction",
    method = "POST"
  )
  @RequestBody(
    description = "Transaction data including type and optional timestamp in milliseconds",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[String])
    )),
    required = true
  )
  def sign: Route = (pathPrefix("sign") & post & withAuth) {
    pathEndOrSingleSlash {
      handleExceptions(jsonExceptionHandler) {
        json[JsObject] { jsv =>
          signTransaction((jsv \ "sender").as[String], jsv)
        }
      }
    } ~ signWithSigner
  }

  @Path("/sign/{signerAddress}")
  @Operation(
    summary = "Sign a transaction by a private key of signer address",
    method = "POST"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "signerAddress",
        description = "Wallet address",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      )
    )
  )
  @RequestBody(
    description = "Transaction data including type and optional timestamp in milliseconds",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[String])
    )),
    required = true
  )
  def signWithSigner: Route = pathPrefix(Segment) { signerAddress =>
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] { jsv =>
        signTransaction(signerAddress, jsv)
      }
    }
  }

  private def signTransaction(signerAddress: String, jsv: JsObject): ToResponseMarshallable = {
    TxRequest
      .fromJson(jsv)
      .flatMap(_.signTx(wallet, signerAddress, time))
      .fold(ApiError.fromValidationError, _.json())
  }

  @Path("/sponsor")
  @Operation(
    summary = "Sponsor a transaction",
    method = "POST"
  )
  @RequestBody(
    description = "Transaction data including type and optional timestamp in milliseconds",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[String])
    )),
    required = true
  )
  def sponsor: Route = (pathPrefix("sign") & post & withAuth) {
    pathEndOrSingleSlash {
      handleExceptions(jsonExceptionHandler) {
        json[JsObject] { jsv =>
          sponsorTransaction((jsv \ "sponsor").as[String], jsv)
        }
      }
    } ~ signWithSigner
  }

  @Path("/sponsor/{signerAddress}")
  @Operation(
    summary = "Sponsor a transaction by a private key of signer address",
    method = "POST"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "signerAddress",
        description = "Wallet address",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      )
    )
  )
  @RequestBody(
    description = "Transaction data including type and optional timestamp in milliseconds",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[String])
    )),
    required = true
  )
  def sponsorWithSigner: Route = pathPrefix(Segment) { signerAddress =>
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] { jsv =>
        sponsorTransaction(signerAddress, jsv)
      }
    }
  }

  private def sponsorTransaction(signerAddress: String, jsv: JsObject): ToResponseMarshallable = {
    TxRequest
      .fromJson(jsv)
      .flatMap(_.sponsorTx(wallet, signerAddress, time))
      .fold(ApiError.fromValidationError, _.json())
  }

  @Path("/broadcast")
  @Operation(
    summary = "Broadcasts a signed transaction",
    method = "POST"
  )
  @RequestBody(
    description = "Transaction data including type and signature",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[String])
    )),
    required = true
  )
  def broadcast: Route = (pathPrefix("broadcast") & post) {
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] {
        createTransaction(_) { tx =>
          doBroadcast(tx)
        }
      }
    }
  }

  private def createTransaction(jsv: JsObject)(f: Transaction => ToResponseMarshallable): ToResponseMarshallable = {
    TxRequest
      .fromJson(jsv)
      .flatMap(_.toTx)
      .flatMap(TransactionsApiRoute.ifPossible(blockchain, _))
      .fold(ApiError.fromValidationError, tx => f(tx))
  }

  private def txToExtendedJson(tx: Transaction): JsObject = {
    import com.ltonetwork.transaction.lease.LeaseTransaction
    tx match {
      case lease: LeaseTransaction =>
        import com.ltonetwork.transaction.lease.LeaseTransaction.Status._
        lease.json() ++ Json.obj("status" -> (if (blockchain.leaseDetails(lease.id()).exists(_.isActive)) Active else Canceled))
      case leaseCancel: CancelLeaseTransaction =>
        leaseCancel.json() ++ Json.obj("lease" -> blockchain.transactionInfo(leaseCancel.leaseId).map(_._2.json()).getOrElse[JsValue](JsNull))
      case t => t.json()
    }
  }

  /**
    * Produces compact representation for large transactions by stripping unnecessary data.
    * Currently implemented for MassTransfer transaction only.
    */
  private def txToCompactJson(address: Address, tx: Transaction): JsObject = {
    import com.ltonetwork.transaction.transfer._
    tx match {
      case mtt: MassTransferTransaction if mtt.sender.toAddress != address =>
        mtt.compactJson(Set(address))
      case _ => txToExtendedJson(tx)
    }
  }

  private val jsonExceptionHandler = ExceptionHandler {
    case JsResultException(err)    => complete(WrongJson(errors = err))
    case e: NoSuchElementException => complete(WrongJson(Some(e)))
  }
}

object TransactionsApiRoute {
  val MaxTransactionsPerRequest = 10000
  def ifPossible[T <: Transaction](bc: Blockchain, tx: T): Either[GenericError, T] = {
    tx match {
      case atx: IssueAssociationTransaction =>
        Either.cond(!bc.assocExists(atx), tx, GenericError("The exact same association already exists"))
      case atx: RevokeAssociationTransaction =>
        Either.cond(bc.assocExists(atx), tx, GenericError("The association doesn't exist"))
      case ltx: CancelLeaseTransaction =>
        Either.cond(bc.leaseDetails(ltx.leaseId).exists(_.isActive), tx, GenericError("The lease is not active"))
      case _ => Right(tx)
    }
  }
}
