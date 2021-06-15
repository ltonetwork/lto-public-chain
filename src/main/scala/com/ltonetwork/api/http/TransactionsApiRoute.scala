package com.ltonetwork.api.http

import java.util.NoSuchElementException
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.api.http.requests.anchor._
import com.ltonetwork.api.http.requests.association._
import com.ltonetwork.api.http.requests.data._
import com.ltonetwork.api.http.requests.lease._
import com.ltonetwork.api.http.requests.smart._
import com.ltonetwork.api.http.requests.sponsorship._
import com.ltonetwork.api.http.requests.transfer._
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.http.BroadcastRoute
import com.ltonetwork.settings.{FeesSettings, FunctionalitySettings, RestAPISettings}
import com.ltonetwork.state.diffs.CommonValidation
import com.ltonetwork.state.{Blockchain, ByteStr}
import com.ltonetwork.transaction.ValidationError.{ActivationError, GenericError}
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.lease._
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.Time
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._

import javax.ws.rs.Path
import play.api.libs.json._

import scala.util.Success
import scala.util.control.Exception

@Path("/transactions")
@Api(value = "/transactions")
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

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info ~ sign ~ calculateFee ~ broadcast
    }

  private val invalidLimit = StatusCodes.BadRequest -> Json.obj("message" -> "invalid.limit")

  //TODO implement general pagination
  @Path("/address/{address}/limit/{limit}")
  @ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "limit",
                           value = "Specified number of records to be returned",
                           required = true,
                           dataType = "integer",
                           paramType = "path")
    ))
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
  @ApiOperation(value = "Info", notes = "Get transaction info", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "transaction id ", required = true, dataType = "string", paramType = "path")
    ))
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
  @ApiOperation(value = "Unconfirmed", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  def unconfirmed: Route = (pathPrefix("unconfirmed") & get) {
    pathEndOrSingleSlash {
      complete(JsArray(utx.all.map(txToExtendedJson)))
    } ~ utxSize ~ utxTransactionInfo
  }

  @Path("/unconfirmed/size")
  @ApiOperation(value = "Size of UTX pool", notes = "Get number of unconfirmed transactions in the UTX pool", httpMethod = "GET")
  def utxSize: Route = (pathPrefix("size") & get) {
    complete(Json.obj("size" -> JsNumber(utx.size)))
  }

  @Path("/unconfirmed/info/{id}")
  @ApiOperation(value = "Transaction Info", notes = "Get transaction that is in the UTX", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "Transaction id ", required = true, dataType = "string", paramType = "path")
    ))
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
  @ApiOperation(value = "Calculate fee", notes = "Calculates a fee for a transaction", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "json",
                           required = true,
                           dataType = "string",
                           paramType = "body",
                           value = "Transaction data including type and optional timestamp in milliseconds")
    ))
  def calculateFee: Route = (pathPrefix("calculateFee") & post) {
    pathEndOrSingleSlash {
      handleExceptions(jsonExceptionHandler) {
        json[JsObject] { jsv =>
          val senderPk = (jsv \ "senderPublicKey").as[String]
          // Just for converting the request to the transaction
          val enrichedJsv = jsv ++ Json.obj(
            "fee"    -> 1234567,
            "sender" -> senderPk
          )
          createTransaction(senderPk, enrichedJsv) { tx =>
            val minFees = CommonValidation.getMinFee(blockchain, functionalitySettings, blockchain.height, tx)
            val r1 = minFees.map {
              case assetAmount =>
                val utxMinFee = new FeeCalculator(feesSettings, blockchain).map.getOrElse(tx.builder.typeId.toInt.toString, 0L)
                val minFee    = Math.max(utxMinFee, assetAmount)
                Json.obj(
//                  "feeAssetId" -> null,
                  "feeAmount" -> minFee
                )
            }
            r1
          }
        }
      }
    }
  }

  @Path("/sign")
  @ApiOperation(value = "Sign a transaction", notes = "Sign a transaction", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "json",
                           required = true,
                           dataType = "string",
                           paramType = "body",
                           value = "Transaction data including type and optional timestamp in milliseconds")
    ))
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
  @ApiOperation(value = "Sign a transaction by a private key of signer address", notes = "Sign a transaction", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "signerAddress", value = "Wallet address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "json",
                           required = true,
                           dataType = "string",
                           paramType = "body",
                           value = "Transaction data including type and optional timestamp in milliseconds")
    ))
  def signWithSigner: Route = pathPrefix(Segment) { signerAddress =>
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] { jsv =>
        signTransaction(signerAddress, jsv)
      }
    }
  }

  private def signTransaction(signerAddress: String, jsv: JsObject): ToResponseMarshallable = {
    val typeId = (jsv \ "type").as[Byte]

    (jsv \ "version").validateOpt[Byte](versionReads) match {
      case JsError(errors) => WrongJson(None, errors)
      case JsSuccess(value, _) =>
        val version = value getOrElse (1: Byte)
        val txJson  = jsv ++ Json.obj("version" -> version)

        (TransactionBuilders.by(typeId, version) match {
          case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
          case Some(x) =>
            (x, version) match {
              case (AnchorTransaction, 1) => TransactionFactory.anchor(txJson.as[AnchorV1Request], wallet, signerAddress, time)
              case (IssueAssociationTransaction, 1) =>
                TransactionFactory
                  .issueAssociation(txJson.as[IssueAssociationV1Request], wallet, signerAddress, time)
                  .flatMap(TransactionsApiRoute.ifPossible(blockchain, _))
              case (RevokeAssociationTransaction, 1) =>
                TransactionFactory
                  .revokeAssociation(txJson.as[IssueAssociationV1Request], wallet, signerAddress, time)
                  .flatMap(TransactionsApiRoute.ifPossible(blockchain, _))
              case (SponsorshipTransaction, 1) => TransactionFactory.sponsorship(txJson.as[SponsorshipV1Request], wallet, signerAddress, time)
              case (CancelSponsorshipTransaction, 1) =>
                TransactionFactory.cancelSponsorship(txJson.as[SponsorshipV1Request], wallet, signerAddress, time)
              case (TransferTransaction, 1)     => TransactionFactory.transferAssetV1(txJson.as[TransferV1Request], wallet, signerAddress, time)
              case (TransferTransaction, 2)     => TransactionFactory.transferAssetV2(txJson.as[TransferV2Request], wallet, signerAddress, time)
              case (MassTransferTransaction, 1) => TransactionFactory.massTransferAsset(txJson.as[MassTransferV1Request], wallet, signerAddress, time)
              case (LeaseTransaction, 1)        => TransactionFactory.leaseV1(txJson.as[LeaseV1Request], wallet, signerAddress, time)
              case (LeaseTransaction, 2)        => TransactionFactory.leaseV2(txJson.as[LeaseV2Request], wallet, signerAddress, time)
              case (CancelLeaseTransaction, 1)  => TransactionFactory.leaseCancelV1(txJson.as[CancelLeaseV1Request], wallet, signerAddress, time)
              case (CancelLeaseTransaction, 2)  => TransactionFactory.leaseCancelV2(txJson.as[CancelLeaseV2Request], wallet, signerAddress, time)
              case (DataTransaction, 1)         => TransactionFactory.data(txJson.as[DataV1Request], wallet, signerAddress, time)
              case (SetScriptTransaction, 1)    => TransactionFactory.setScript(txJson.as[SetScriptV1Request], wallet, signerAddress, time)
              case _                            => Left(GenericError(s"Unsupported transaction type ($typeId) and version ($version)"))
            }
        }).fold(ApiError.fromValidationError, _.json())
    }
  }

  private def createTransaction(senderPk: String, jsv: JsObject)(f: Transaction => ToResponseMarshallable): ToResponseMarshallable = {
    val typeId = (jsv \ "type").as[Byte]

    (jsv \ "version").validateOpt[Byte](versionReads) match {
      case JsError(errors) => WrongJson(None, errors)
      case JsSuccess(value, _) =>
        val version = value.getOrElse(1: Byte)
        val txJson  = jsv ++ Json.obj("version" -> version)

        PublicKeyAccount
          .fromBase58String(senderPk)
          .flatMap { senderPk =>
            TransactionBuilders.by(typeId, version) match {
              case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
              case Some(x) =>
                (x, version) match {
                  case (AnchorTransaction, 1) => TransactionFactory.anchor(txJson.as[AnchorV1Request], senderPk)
                  case (IssueAssociationTransaction, 1) =>
                    TransactionFactory
                      .issueAssociation(txJson.as[IssueAssociationV1Request], senderPk)
                      .flatMap(TransactionsApiRoute.ifPossible(blockchain, _))
                  case (RevokeAssociationTransaction, 1) =>
                    TransactionFactory
                      .revokeAssociation(txJson.as[IssueAssociationV1Request], senderPk)
                      .flatMap(TransactionsApiRoute.ifPossible(blockchain, _))
                  case (SponsorshipTransaction, 1)       => TransactionFactory.sponsorship(txJson.as[SponsorshipV1Request], senderPk)
                  case (CancelSponsorshipTransaction, 1) => TransactionFactory.cancelSponsorship(txJson.as[SponsorshipV1Request], senderPk)
                  case (TransferTransaction, 1)        => TransactionFactory.transferAssetV1(txJson.as[TransferV1Request], senderPk)
                  case (TransferTransaction, 2)        => TransactionFactory.transferAssetV2(txJson.as[TransferV2Request], senderPk)
                  case (MassTransferTransaction, 1)    => TransactionFactory.massTransferAsset(txJson.as[MassTransferV1Request], senderPk)
                  case (LeaseTransaction, 1)           => TransactionFactory.leaseV1(txJson.as[LeaseV1Request], senderPk)
                  case (CancelLeaseTransaction, 1)     => TransactionFactory.leaseCancelV1(txJson.as[CancelLeaseV1Request], senderPk)
                  case (LeaseTransaction, 2)           => TransactionFactory.leaseV2(txJson.as[LeaseV2Request], senderPk)
                  case (CancelLeaseTransaction, 2)     => TransactionFactory.leaseCancelV2(txJson.as[CancelLeaseV2Request], senderPk)
                  case (DataTransaction, 1)            => TransactionFactory.data(txJson.as[DataV1Request], senderPk)
                  case (SetScriptTransaction, 1)       => TransactionFactory.setScript(txJson.as[SetScriptV1Request], senderPk)
                  case _                               => Left(GenericError(s"Unsupported transaction type ($typeId) and version ($version)"))
                }
            }
          }
          .fold(ApiError.fromValidationError, tx => f(tx))
    }
  }

  @Path("/broadcast")
  @ApiOperation(value = "Broadcasts a signed transaction", notes = "Broadcasts a signed transaction", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "json",
                           value = "Transaction data including type and signature",
                           required = true,
                           dataType = "string",
                           paramType = "body")
    ))
  def broadcast: Route = (pathPrefix("broadcast") & post) {
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] { jsv =>
        val typeId  = (jsv \ "type").as[Byte]
        val version = (jsv \ "version").asOpt[Byte](versionReads).getOrElse(1.toByte)

        implicit val broadcastAnchorRequestReadsFormat: Format[SignedAnchorV1Request]           = Json.format
        implicit val broadcastAssocRequestReadsFormat: Format[SignedIssueAssociationV1Request]  = Json.format
        implicit val broadcastSponsorshipRequestReadsFormat: Format[SignedSponsorshipV1Request]   = Json.format

        val r: Either[ValidationError, Transaction] = TransactionBuilders.by(typeId, version) match {
          case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
          case Some(x) =>
            (x, version) match {
              case (AnchorTransaction, 1) => jsv.as[SignedAnchorV1Request].toTx
              case (IssueAssociationTransaction, 1) => jsv.as[SignedIssueAssociationV1Request].toTx.flatMap(TransactionsApiRoute.ifPossible(blockchain, _))
              case (RevokeAssociationTransaction, 1) => jsv.as[SignedIssueAssociationV1Request].toTx.flatMap(TransactionsApiRoute.ifPossible(blockchain, _))
              case (SponsorshipTransaction, 1)       => jsv.as[SignedSponsorshipV1Request].toTx
              case (CancelSponsorshipTransaction, 1) => jsv.as[SignedSponsorshipV1Request].toTx
              case (TransferTransaction, 1)          => jsv.as[SignedTransferV1Request].toTx
              case (TransferTransaction, 2)          => jsv.as[SignedTransferV2Request].toTx
              case (MassTransferTransaction, 1)      => jsv.as[SignedMassTransferV1Request].toTx
              case (LeaseTransaction, 1)             => jsv.as[SignedLeaseV1Request].toTx
              case (LeaseTransaction, 2)             => jsv.as[SignedLeaseV2Request].toTx
              case (CancelLeaseTransaction, 1)       => jsv.as[SignedCancelLeaseV1Request].toTx
              case (CancelLeaseTransaction, 2)       => jsv.as[SignedCancelLeaseV2Request].toTx
              case (DataTransaction, 1)              => jsv.as[SignedDataV1Request].toTx
              case (SetScriptTransaction, 1)         => jsv.as[SignedSetScriptV1Request].toTx
              case _                            => Left(GenericError(s"Unsupported transaction type ($typeId) and version ($version)"))
            }
        }
        import com.ltonetwork.features.FeatureProvider._
        val r0 = r match {
          case Right(tx)
              if tx.typeId == SetScriptTransaction.typeId &&
                !blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts, blockchain.height) =>
            Left(ActivationError("SmartAccounts feature has not been activated yet"))
          case x => x
        }
        doBroadcast(r0)
      }
    }
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
  def ifPossible(bc: Blockchain, tx: AssociationTransaction): Either[GenericError, AssociationTransaction] = {
    tx match {
      case _: IssueAssociationTransaction =>
        if (bc.assocExists(tx)) Left(GenericError("The exact same association already exists")) else Right(tx)
      case _: RevokeAssociationTransaction =>
        if (!bc.assocExists(tx)) Left(GenericError("The association doesn't exist")) else Right(tx)
    }
  }
}
