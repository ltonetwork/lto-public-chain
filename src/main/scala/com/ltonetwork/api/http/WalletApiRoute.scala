package com.ltonetwork.api.http

import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.api.http.AddressApiRoute.Signed
import com.ltonetwork.crypto
import com.ltonetwork.settings.RestAPISettings
import play.api.libs.json.{JsArray, JsBoolean, JsString, Json}
import com.ltonetwork.utils.Base58
import com.ltonetwork.wallet.Wallet
import jakarta.validation.Path
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.responses.{ApiResponse, ApiResponses}
import io.swagger.v3.oas.annotations.tags.Tag

import java.nio.charset.StandardCharsets

@Path("/wallet")
@Tag("wallet")
case class WalletApiRoute(settings: RestAPISettings, wallet: Wallet) extends ApiRoute {

  override lazy val route =
    pathPrefix("wallet") {
      addresses ~ deleteAddress ~ createAddress ~ seq ~ sign ~ signText
    }

  val MaxAddressesPerRequest = 1000

  @Path("/addresses")
  @Operation(
    summary = "Get wallet accounts addresses",
    method = "GET"
  )
  def addresses: Route = (path("addresses") & get) {
    val accounts = wallet.privateKeyAccounts
    val json     = JsArray(accounts.map(a => JsString(a.address)))
    complete(json)
  }

  @Path("/addresses")
  @Operation(
    summary = "Create a new account in the wallet (if it exists)",
    method = "POST"
  )
  def createAddress: Route = (path("addresses") & post & withAuth) {
    wallet.generateNewAccount() match {
      case Right(Some(pka)) => complete(Json.obj("address" -> pka.address))
      case Right(None)      => complete(Unknown)
      case Left(e)          => complete(e)
    }
  }

  @Path("/addresses/{address}")
  @Operation(
    summary = "Remove the account with address from the wallet",
    method = "DELETE"
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
  def deleteAddress: Route = path("addresses" / Segment) { address =>
    (delete & withAuth) {
      if (Address.fromString(address).isLeft) {
        complete(InvalidAddress)
      } else {
        val deleted = wallet.findPrivateKey(address).flatMap(account => wallet.deleteAccount(account))
        deleted match {
          case Right(d) => complete(Json.obj("deleted" -> JsBoolean(d)))
          case Left(e)  => complete(e)
        }
      }
    }
  }

  @Path("/addresses/seq/{from}/{to}")
  @Operation(
    summary = "Get wallet accounts addresses",
    method = "GET"
  )
  @Parameters(
    Array(
      new Parameter(
        name = "from",
        description = "Start wallet address",
        required = true,
        schema = new Schema(implementation = classOf[Int]),
        in = ParameterIn.PATH
      ),
      new Parameter(
        name = "to",
        description = "End wallet address",
        required = true,
        schema = new Schema(implementation = classOf[Int]),
        in = ParameterIn.PATH
      )
    )
  )
  def seq: Route = {
    (path("addresses" / "seq" / IntNumber / IntNumber) & get) {
      case (start, end) =>
        if (start >= 0 && end >= 0 && start - end < MaxAddressesPerRequest) {
          val json = JsArray(
            wallet.privateKeyAccounts.map(a => JsString(a.address)).slice(start, end)
          )

          complete(json)
        } else complete(TooBigArrayAllocation)
    }
  }

  @Path("/sign/{address}")
  @Operation(
    summary = "Sign a message with a private key associated with address",
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
    description = "Message to sign as a plain string",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[String]),
    )),
    required = true
  )
  @ApiResponses(
    Array(
      new ApiResponse(
        responseCode = "200",
        description =
          "Json with error or json like {\"message\": \"Base58-encoded\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}"
      )
    ))
  def sign: Route = {
    path("sign" / Segment) { address =>
      signPath(address, encode = true)
    }
  }

  @Path("/signText/{address}")
  @Operation(
    summary = "Sign a message with a private key associated with address",
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
    description = "Message to sign as a plain string",
    content = Array(new Content(
      schema = new Schema(implementation = classOf[String]),
    )),
    required = true
  )
  @ApiResponses(
    Array(
      new ApiResponse(
        responseCode = "200",
        description = "Json with error or json like {\"message\": \"plain text\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}")
    ))
  def signText: Route = {
    path("signText" / Segment) { address =>
      signPath(address, encode = false)
    }
  }

  private def signPath(address: String, encode: Boolean) = (post & entity(as[String])) { message =>
    withAuth {
      val res = wallet
        .findPrivateKey(address)
        .map(pk => {
          val messageBytes = message.getBytes(StandardCharsets.UTF_8)
          val signature    = crypto.sign(pk, messageBytes)
          val msg          = if (encode) Base58.encode(messageBytes) else message
          Signed(msg, Base58.encode(pk.publicKey), Base58.encode(signature))
        })
      complete(res)
    }
  }
}
