package com.ltonetwork.api.http.swagger

import akka.actor.ActorSystem
import com.github.swagger.akka.model.{Info, License}
import com.github.swagger.akka.SwaggerHttpService
import com.ltonetwork.Version
import com.ltonetwork.settings.RestAPISettings
import io.swagger.v3.oas.models.info.Info
import io.swagger.v3.oas.models.servers.{Server, ServerVariable, ServerVariables}
import io.swagger.v3.oas.models.OpenAPI

class SwaggerDocService(val actorSystem: ActorSystem, val apiClasses: Set[Class[_]], settings: RestAPISettings)
    extends SwaggerHttpService {

  def createServer(scheme: String): Server = {
    val swaggerServer: Server = new Server()
    swaggerServer.setUrl(basePath)
    val httpServerSchemes: ServerVariables = new ServerVariables()
    httpServerSchemes.addServerVariable("scheme", new ServerVariable()._default(scheme))
    swaggerServer.setVariables(httpServerSchemes)
    swaggerServer
  }

  override val host: String = settings.bindAddress + ":" + settings.port
  override val info: Info = Info(
    "The Web Interface to the LTO Public Node API",
    Version.VersionString,
    "LTO Public Full Node",
    "License: Apache License, Version 2.0",
    None,
    Some(License("Apache License, Version 2.0", "https://github.com/legalthings/PublicNode/blob/master/LICENSE"))
  )

  //Let swagger-ui determine the host and port
  override val swaggerConfig: OpenAPI = new OpenAPI()
    .addServersItem(createServer("http"))
    .addServersItem(createServer("https"))
    .info(info)
}
