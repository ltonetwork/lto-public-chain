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
import jakarta.ws.rs.Path
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.tags.Tag
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

@Path("/addresses")
@Tag(name="addresses")
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
  @Operation(
    summary = "Account's script",
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
  def scriptInfo: Route = (path("scriptInfo" / Segment) & get) { address =>
    complete(
      Address
        .fromString(address)
        .flatMap(addressScriptInfoJson)
        .map(ToResponseMarshallable(_))
    )
  }

  @Path("/verify/{address}")
  @Operation(
    summary = "Check a signature of a message signed by an account",
    method = "POST"
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
  @RequestBody(
    description = "Json with data",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[com.ltonetwork.api.http.SignedMessage]),
      examples = Array(new ExampleObject(
        value = "{\n\t\"message\":\"Base58-encoded message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
      ))
    )),
    required = true
  )
  def verify: Route = {
    path("verify" / Segment) { address =>
      verifyPath(address, decode = true)
    }
  }

  @Path("/verifyText/{address}")
  @Operation(
    summary = "Check a signature of a message signed by an account",
    method = "POST"
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
  @RequestBody(
    description = "Json with data",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[com.ltonetwork.api.http.SignedMessage]),
      examples = Array(new ExampleObject(
        value = "{\n\t\"message\":\"Plain message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}",
      ))
    )),
    required = true
  )
  def verifyText: Route = path("verifyText" / Segment) { address =>
    verifyPath(address, decode = false)
  }

  @Path("/balance/{address}")
  @Operation(
    summary = "Account's balance",
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
  def balance: Route = (path("balance" / Segment) & get) { address =>
    complete(balanceJson(address))
  }

  @Path("/balance/details/{address}")
  @Operation(
    summary = "Account's balances",
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
  @Operation(
    summary = "Balance history of address",
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
  def balanceHistory: Route = {
    (path("balance" / "history" / Segment) & get) { address =>
      complete(balanceHistoryJson(address));
    }
  }

  @Path("/balance/{address}/{confirmations}")
  @Operation(
    summary = "Balance of address after confirmations",
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
        name = "address",
        description = "Number of confirmations",
        required = true,
        schema = new Schema(implementation = classOf[Int]),
        in = ParameterIn.PATH
      )
    )
  )
  def balanceWithConfirmations: Route = {
    (path("balance" / Segment / IntNumber) & get) {
      case (address, confirmations) =>
        complete(balanceJson(address, confirmations))
    }
  }

  @Path("/effectiveBalance/{address}")
  @Operation(
    summary = "Account's effective balance",
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
  def effectiveBalance: Route = {
    path("effectiveBalance" / Segment) { address =>
      complete(effectiveBalanceJson(address, 0))
    }
  }

  @Path("/effectiveBalance/{address}/{confirmations}")
  @Operation(
    summary = "Effective balance of address after confirmations",
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
        name = "address",
        description = "Number of confirmations",
        required = true,
        schema = new Schema(implementation = classOf[Int]),
        in = ParameterIn.PATH
      )
    )
  )
  def effectiveBalanceWithConfirmations: Route = {
    path("effectiveBalance" / Segment / IntNumber) {
      case (address, confirmations) =>
        complete(
          effectiveBalanceJson(address, confirmations)
        )
    }
  }

  @Path("/validate/{address}")
  @Operation(
    summary = "Check whether address is valid or not",
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
  def validate: Route = (path("validate" / Segment) & get) { address =>
    complete(Validity(address, Address.fromString(address).isRight))
  }

  @Path("/")
  @Operation(
    summary = "Get wallet accounts addresses; deprecated: use `/wallet/addresses` instead",
    method = "GET",
    deprecated = true
  )
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
  @Operation(
    summary = "Generate an address from public key",
    method = "GET"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "publicKey",
        description = "Public key Base58-encoded",
        required = true,
        schema = new Schema(implementation = classOf[String]),
        in = ParameterIn.PATH
      )
    )
  )
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
