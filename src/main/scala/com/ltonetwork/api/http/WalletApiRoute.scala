package com.ltonetwork.api.http

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.api.http.AddressApiRoute.Signed
import com.ltonetwork.crypto
import com.ltonetwork.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json.{JsArray, JsBoolean, JsString, Json}
import com.ltonetwork.utils.Base58
import com.ltonetwork.wallet.Wallet

import java.nio.charset.StandardCharsets

@Path("/wallet")
@Api(value = "/wallet")
case class WalletApiRoute(settings: RestAPISettings, wallet: Wallet) extends ApiRoute {

  override lazy val route =
    pathPrefix("wallet") {
      addresses ~ deleteAddress ~ createAddress ~ seq ~ sign ~ signText
    }

  val MaxAddressesPerRequest = 1000

  @Path("/addresses")
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses", httpMethod = "GET")
  def addresses: Route = (path("addresses") & get) {
    val accounts = wallet.privateKeyAccounts
    val json     = JsArray(accounts.map(a => JsString(a.address)))
    complete(json)
  }

  @Path("/addresses")
  @ApiOperation(value = "Create", notes = "Create a new account in the wallet (if it exists)", httpMethod = "POST")
  def createAddress: Route = (path("addresses") & post & withAuth) {
    wallet.generateNewAccount() match {
      case Right(Some(pka)) => complete(Json.obj("address" -> pka.address))
      case Right(None)      => complete(Unknown)
      case Left(e)          => complete(e)
    }
  }

  @Path("/addresses/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the account with address {address} from the wallet", httpMethod = "DELETE")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def deleteAddress: Route = path(Segment) { address =>
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
  @ApiOperation(value = "Seq", notes = "Get wallet accounts addresses", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "from", value = "Start address", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "to", value = "address", required = true, dataType = "integer", paramType = "path")
    ))
  def seq: Route = {
    (path("seq" / IntNumber / IntNumber) & get) {
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
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "string"),
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  @ApiResponses(
    Array(
      new ApiResponse(
        code = 200,
        message =
          "Json with error or json like {\"message\": \"Base58-encoded\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}"
      )
    ))
  def sign: Route = {
    path("sign" / Segment) { address =>
      signPath(address, encode = true)
    }
  }

  @Path("/signText/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "string"),
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  @ApiResponses(
    Array(
      new ApiResponse(
        code = 200,
        message = "Json with error or json like {\"message\": \"plain text\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}")
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
