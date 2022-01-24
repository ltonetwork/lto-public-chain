package com.ltonetwork.fee.api

import akka.http.scaladsl.server.{ExceptionHandler, Route}
import com.ltonetwork.api.http.{ApiRoute, WrongJson}
import com.ltonetwork.fee.FeeVoteStatus
import com.ltonetwork.mining.MinerOptions
import com.ltonetwork.settings.{FunctionalitySettings, RestAPISettings}
import com.ltonetwork.state.Blockchain
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.media.{Content, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.tags.Tag
import jakarta.ws.rs.{GET, POST, Path}
import play.api.libs.json.{JsNumber, JsObject, JsResultException, JsString, Json}

import java.util.NoSuchElementException

@Path("/fees")
@Tag(name = "fees")
case class FeesApiRoute(settings: RestAPISettings,
                        blockchain: Blockchain,
                        functionalitySettings: FunctionalitySettings,
                        minerOptions: MinerOptions)
  extends ApiRoute {

  override lazy val route =
    pathPrefix("fees") {
      status ~ vote
    }

  @GET
  @Path("/status")
  @Operation(
    summary = "Get status of the fee price"
  )
  def status: Route = (path("status") & get) {
    val price = blockchain.feePrice
    val next = FeeVoteStatus.Unchanged

    complete(Json.obj(
      "price" -> price,
      "votes" -> 0,
      "nodeStatus" -> minerOptions.feeVote.description,
      "next" -> Json.obj(
        "status" -> next.description,
        "price" -> next.calc(price),
        "activationHeight" -> nextPeriod
      )
    ))
  }

  @POST
  @Path("/vote")
  @Operation(
    summary = "Vote for changing the fee price"
  )
  @RequestBody(
    description = "Voting status",
    content = Array(
      new Content(
        schema = new Schema(implementation = classOf[String])
      )),
    required = true
  )
  def vote: Route = (path("vote") & post) {
    handleExceptions(jsonExceptionHandler) {
      /*json[JsObject] { jsv =>
        minerOptions.feeVote = (jsv \ "status").get match {
          case JsString(d) => FeeVoteStatus(d)
          case JsNumber(v) => FeeVoteStatus(v.toByte)
          case _           => throw new RuntimeException("Invalid type of status property")
        }

        Json.obj("status" -> minerOptions.feeVote.description)
      }*/
      minerOptions.feeVote = FeeVoteStatus.Increase
      complete(Json.obj("status" -> minerOptions.feeVote.description))
    }
  }

  private val jsonExceptionHandler = ExceptionHandler {
    case JsResultException(err)    => complete(WrongJson(errors = err))
    case e: RuntimeException       => complete(WrongJson(Some(e)))
  }

  private def nextPeriod: Int = blockchain.height +
    functionalitySettings.feeVoteBlocksPeriod - blockchain.height % functionalitySettings.feeVoteBlocksPeriod
}
