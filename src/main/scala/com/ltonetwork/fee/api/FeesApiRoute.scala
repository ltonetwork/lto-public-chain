package com.ltonetwork.fee.api

import akka.http.scaladsl.server.{ExceptionHandler, Route}
import com.ltonetwork.api.http.{ApiRoute, WrongJson}
import com.ltonetwork.fee.FeeVoteStatus
import com.ltonetwork.settings.{FunctionalitySettings, RestAPISettings}
import com.ltonetwork.state.Blockchain
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.tags.Tag
import jakarta.ws.rs.{GET, POST, Path}
import play.api.libs.json.{JsObject, JsResultException, Json}

import java.util.NoSuchElementException

@Path("/fees")
@Tag(name = "fees")
case class FeesApiRoute(settings: RestAPISettings,
                        blockchain: Blockchain,
                        functionalitySettings: FunctionalitySettings)
  extends ApiRoute {

  override lazy val route =
    pathPrefix("fees") {
      status
    }

  @GET
  @Path("/status")
  @Operation(
    summary = "Get status of the current fee price"
  )
  def status: Route = (path("status") & get) {
    val price = blockchain.feePrice
    val next = FeeVoteStatus.Unchanged

    complete(Json.obj(
      "price" -> price,
      "votes" -> 0,
      "nodeStatus" -> FeeVoteStatus.Unchanged.description,
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
  def vote: Route = (path("vote") & post & withAuth) {
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] { jsv =>
        val vote = FeeVoteStatus((jsv \ "status").as[String])

        // TODO store node vote

        Json.obj("status" -> vote.description)
      }
    }
  }

  private val jsonExceptionHandler = ExceptionHandler {
    case JsResultException(err)    => complete(WrongJson(errors = err))
    case e: NoSuchElementException => complete(WrongJson(Some(e)))
  }

  private def nextPeriod: Int = blockchain.height +
    functionalitySettings.feeVoteBlocksPeriod - blockchain.height % functionalitySettings.feeVoteBlocksPeriod
}
