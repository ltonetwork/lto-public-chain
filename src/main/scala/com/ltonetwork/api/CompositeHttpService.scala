package com.ltonetwork.api

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult.Complete
import akka.http.scaladsl.server.directives.{DebuggingDirectives, LoggingMagnet}
import akka.http.scaladsl.server.{Directive0, Route, RouteResult}
import com.github.swagger.akka.CustomMediaTypes
import com.github.swagger.akka.SwaggerHttpService.apiDocsBase
import com.ltonetwork.api.swagger.SwaggerDocService
import com.ltonetwork.settings.RestAPISettings
import com.ltonetwork.utils.ScorexLogging

case class CompositeHttpService(system: ActorSystem, apiTypes: Set[Class[_]], routes: Seq[ApiRoute], settings: RestAPISettings)
    extends ScorexLogging {

  val swaggerService = new SwaggerDocService(system, apiTypes, settings)

  def withCors: Directive0 =
    if (settings.cors)
      respondWithHeader(`Access-Control-Allow-Origin`.*)
    else pass

  private val headers: scala.collection.immutable.Seq[String] =
    scala.collection.immutable.Seq("Authorization", "Content-Type", "X-Requested-With", "Timestamp", "Signature") ++
    (if (settings.apiKeyDifferentHost) Seq("X-API-Key") else Seq.empty[String])

  val compositeRoute: Route =
    withCors(routes.map(_.route).reduce(_ ~ _)) ~
      (pathEndOrSingleSlash | path("swagger")) {
        redirect("/api-docs/index.html", StatusCodes.PermanentRedirect)
      } ~
      pathPrefix("api-docs") {
        pathEndOrSingleSlash {
          redirect("/api-docs/index.html", StatusCodes.PermanentRedirect)
        } ~
        path("swagger.json") {
          complete(HttpEntity(MediaTypes.`application/json`, swaggerService.swaggerJson))
        } ~
        path("swagger.yaml") {
          complete(HttpEntity(CustomMediaTypes.`text/vnd.yaml`, swaggerService.swaggerYaml))
        } ~
        getFromResourceDirectory("swagger-ui")
      } ~ options {
      respondWithDefaultHeaders(`Access-Control-Allow-Credentials`(true),
                                `Access-Control-Allow-Headers`(headers),
                                `Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE))(withCors(complete(StatusCodes.OK)))
    } ~ complete(StatusCodes.NotFound)

  def logRequestResponse(req: HttpRequest)(res: RouteResult): Unit = res match {
    case Complete(resp) =>
      val msg = s"HTTP ${resp.status.value} from ${req.method.value} ${req.uri}"
      if (resp.status.intValue() < 400) log.debug(msg)
      else if (resp.status.intValue() < 500) log.info(msg)
      else log.warn(msg)
    case _ =>
  }

  val loggingCompositeRoute: Route =
    DebuggingDirectives.logRequestResult(LoggingMagnet(_ => logRequestResponse))(compositeRoute)
}
