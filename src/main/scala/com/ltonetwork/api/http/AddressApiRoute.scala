package com.ltonetwork.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.consensus.GeneratingBalanceProvider
import com.ltonetwork.crypto
import com.ltonetwork.http.BroadcastRoute
import com.ltonetwork.settings.{FunctionalitySettings, RestAPISettings}
import com.ltonetwork.state.Blockchain
import com.ltonetwork.state.diffs.CommonValidation
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.smart.script.ScriptCompiler
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.utils.{Base58, Time}
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._

import javax.ws.rs.Path
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

@Path("/addresses")
@Api(value = "/addresses/")
case class AddressApiRoute(settings: RestAPISettings,
                           wallet: Wallet,
                           blockchain: Blockchain,
                           utx: UtxPool,
                           allChannels: ChannelGroup,
                           time: Time,
                           functionalitySettings: FunctionalitySettings,
                           loadBalanceHistory: Address => Seq[(Int, Long)])
    extends ApiRoute
    with BroadcastRoute {

  import AddressApiRoute._

  override lazy val route: Route =
    pathPrefix("addresses") {
      validate ~ balanceWithConfirmations ~ balanceDetails ~ balanceHistory ~ balance ~
        balanceWithConfirmations ~ verify ~ verifyText ~ publicKey ~
        effectiveBalance ~ effectiveBalanceWithConfirmations ~ scriptInfo
    } ~ root

  @Path("/scriptInfo/{address}")
  @ApiOperation(value = "Details for account", notes = "Account's script", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def scriptInfo: Route = (path("scriptInfo" / Segment) & get) { address =>
    complete(
      Address
        .fromString(address)
        .flatMap(addressScriptInfoJson)
        .map(ToResponseMarshallable(_))
    )
  }

  @Path("/verify/{address}")
  @ApiOperation(value = "Verify", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.ltonetwork.api.http.SignedMessage",
        defaultValue =
          "{\n\t\"message\":\"Base58-encoded message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
      )
    ))
  def verify: Route = {
    path("verify" / Segment) { address =>
      verifyPath(address, decode = true)
    }
  }

  @Path("/verifyText/{address}")
  @ApiOperation(value = "Verify text", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.ltonetwork.api.http.SignedMessage",
        defaultValue =
          "{\n\t\"message\":\"Plain message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
      )
    ))
  def verifyText: Route = path("verifyText" / Segment) { address =>
    verifyPath(address, decode = false)
  }

  @Path("/balance/{address}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def balance: Route = (path("balance" / Segment) & get) { address =>
    complete(balanceJson(address))
  }

  @Path("/balance/details/{address}")
  @ApiOperation(value = "Details for balance", notes = "Account's balances", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def balanceDetails: Route = (path("balance" / "details" / Segment) & get) { address =>
    complete(
      Address
        .fromString(address)
        .right
        .map(acc => {
          ToResponseMarshallable(balancesDetailsJson(acc))
        })
        .getOrElse(InvalidAddress))
  }

  @Path("/balance/history/{address}")
  @ApiOperation(value = "Balance history", notes = "Balance history of {address}", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def balanceHistory: Route = {
    (path("balance" / "history" / Segment) & get) { address =>
      complete(balanceHistoryJson(address));
    }
  }

  @Path("/balance/{address}/{confirmations}")
  @ApiOperation(value = "Confirmed balance", notes = "Balance of {address} after {confirmations}", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "confirmations", value = "0", required = true, dataType = "integer", paramType = "path")
    ))
  def balanceWithConfirmations: Route = {
    (path("balance" / Segment / IntNumber) & get) {
      case (address, confirmations) =>
        complete(balanceJson(address, confirmations))
    }
  }

  @Path("/effectiveBalance/{address}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def effectiveBalance: Route = {
    path("effectiveBalance" / Segment) { address =>
      complete(effectiveBalanceJson(address, 0))
    }
  }

  @Path("/effectiveBalance/{address}/{confirmations}")
  @ApiOperation(value = "Confirmed balance", notes = "Balance of {address} after {confirmations}", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "confirmations", value = "0", required = true, dataType = "integer", paramType = "path")
    ))
  def effectiveBalanceWithConfirmations: Route = {
    path("effectiveBalance" / Segment / IntNumber) {
      case (address, confirmations) =>
        complete(
          effectiveBalanceJson(address, confirmations)
        )
    }
  }

  @Path("/validate/{address}")
  @ApiOperation(value = "Validate", notes = "Check whether address {address} is valid or not", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def validate: Route = (path("validate" / Segment) & get) { address =>
    complete(Validity(address, Address.fromString(address).isRight))
  }

  @Path("/")
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses. Deprecated: use `/wallet/addresses` instead", httpMethod = "GET")
  def root: Route = (path("addresses") & get) {
    redirect("/wallet/addresses", StatusCodes.PermanentRedirect)
  }

  private def balanceJson(address: String, confirmations: Int): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(
        acc =>
          ToResponseMarshallable(
            Balance(
              acc.address,
              confirmations,
              blockchain.balance(acc, blockchain.height, confirmations)
            )))
      .getOrElse(InvalidAddress)
  }

  private def balanceJson(address: String): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(
        acc =>
          ToResponseMarshallable(
            Balance(
              acc.address,
              0,
              blockchain.portfolio(acc).balance
            )))
      .getOrElse(InvalidAddress)
  }

  private def balancesDetailsJson(account: Address): BalanceDetails = {
    val portfolio = blockchain.portfolio(account)
    BalanceDetails(
      account.address,
      portfolio.balance,
      GeneratingBalanceProvider.balance(blockchain, functionalitySettings, account),
      portfolio.balance - portfolio.lease.out,
      portfolio.effectiveBalance
    )
  }

  private def balanceHistoryJson(address: String): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(acc =>
        ToResponseMarshallable(
          loadBalanceHistory(acc).map {
            case (h, b) => Json.obj("height" -> h, "balance" -> b)
          }
      ))
      .getOrElse(InvalidAddress)
  }

  private def addressScriptInfoJson(account: Address): Either[ValidationError, AddressScriptInfo] =
    for {
      script     <- Right(blockchain.accountScript(account))
      complexity <- script.fold[Either[ValidationError, Long]](Right(0))(x => ScriptCompiler.estimate(x).left.map(GenericError(_)))
    } yield
      AddressScriptInfo(
        address = account.address,
        script = script.map(_.bytes().base64),
        scriptText = script.map(_.text),
        complexity = complexity,
        extraFee = if (script.isEmpty) 0 else CommonValidation.ScriptExtraFee
      )

  private def effectiveBalanceJson(address: String, confirmations: Int): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(acc => ToResponseMarshallable(Balance(acc.address, confirmations, blockchain.effectiveBalance(acc, confirmations))))
      .getOrElse(InvalidAddress)
  }

  private def accountData(address: String): ToResponseMarshallable = {
    Address
      .fromString(address)
      .map { acc =>
        ToResponseMarshallable(blockchain.accountData(acc).data.values.toSeq.sortBy(_.key))
      }
      .getOrElse(InvalidAddress)
  }

  private def accountData(address: String, key: String): ToResponseMarshallable = {
    val result = for {
      addr  <- Address.fromString(address).left.map(_ => InvalidAddress)
      value <- blockchain.accountData(addr, key).toRight(DataKeyNotExists)
    } yield value
    ToResponseMarshallable(result)
  }

  private def verifyPath(address: String, decode: Boolean) = withAuth {
    json[SignedMessage] { m =>
      if (Address.fromString(address).isLeft) {
        InvalidAddress
      } else {
        //DECODE SIGNATURE
        val msg: Try[Array[Byte]] = if (decode) Base58.decode(m.message) else Success(m.message.getBytes)
        verifySigned(msg, m.signature, m.publickey, address)
      }
    }
  }

  private def verifySigned(msg: Try[Array[Byte]], signature: String, publicKey: String, address: String) = {
    (msg, Base58.decode(signature), Base58.decode(publicKey)) match {
      case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
        val account = PublicKeyAccount(pubKeyBytes)
        val isValid = account.address == address && crypto.verify(signatureBytes, msgBytes, pubKeyBytes)
        Right(Json.obj("valid" -> isValid))
      case _ => Left(InvalidMessage)
    }
  }

  @Path("/publicKey/{publicKey}")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "publicKey", value = "Public key Base58-encoded", required = true, paramType = "path", dataType = "string")
    ))
  @ApiOperation(value = "Address from Public Key", notes = "Generate a address from public key", httpMethod = "GET")
  def publicKey: Route = (path("publicKey" / Segment) & get) { publicKey =>
    Base58.decode(publicKey) match {
      case Success(pubKeyBytes) =>
        val account = Address.fromPublicKey(pubKeyBytes)
        complete(Json.obj("address" -> account.address))
      case Failure(_) => complete(InvalidPublicKey)
    }
  }
}

object AddressApiRoute {

  case class Signed(message: String, publicKey: String, signature: String)

  implicit val signedFormat: Format[Signed] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)

  implicit val balanceFormat: Format[Balance] = Json.format

  case class BalanceDetails(address: String, regular: Long, generating: Long, available: Long, effective: Long)

  implicit val balanceDetailsFormat: Format[BalanceDetails] = Json.format

  case class Validity(address: String, valid: Boolean)

  implicit val validityFormat: Format[Validity] = Json.format

  case class AddressScriptInfo(address: String, script: Option[String], scriptText: Option[String], complexity: Long, extraFee: Long)

  implicit val accountScriptInfoFormat: Format[AddressScriptInfo] = Json.format
}
