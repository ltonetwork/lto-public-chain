package com.ltonetwork.api.swagger

import akka.actor.ActorSystem
import com.github.swagger.akka.{SwaggerGenerator, SwaggerHttpService}
import com.ltonetwork.Version
import com.ltonetwork.settings.RestAPISettings
import io.swagger.v3.oas.models.info.{Info, License}
import io.swagger.v3.oas.models.security.SecurityScheme
import io.swagger.v3.oas.models.servers.Server
import io.swagger.v3.oas.models.{Components, OpenAPI}

class SwaggerDocService(val actorSystem: ActorSystem, val apiClasses: Set[Class[_]], settings: RestAPISettings) extends SwaggerGenerator {
  import SwaggerHttpService._

  override val host: String = settings.bindAddress + ":" + settings.port

  lazy val swaggerJson = generateSwaggerJson
  lazy val swaggerYaml = generateSwaggerYaml

  val license = new License()
  license.setName("Apache License, Version 2.0")
  license.setUrl("https://github.com/ltonetwork/lto-public-chain/blob/master/LICENSE")

  override val info = new Info()
    .title("LTO Public Node")
    .version(Version.VersionString)
    .description("The Web Interface to the LTO Public Node API")
    .license(license)

  val scheme = new SecurityScheme()
  scheme.setType(SecurityScheme.Type.HTTP)
  scheme.in(SecurityScheme.In.HEADER)
  scheme.setScheme("bearer")
  override val components = Option(new Components().addSecuritySchemes("bearerAuth", scheme))
  override val securitySchemes = Map({ "bearerAuth" -> scheme });

  //Let swagger-ui determine the host and port
  override val swaggerConfig: OpenAPI = new OpenAPI()
    .addServersItem(new Server().url(SwaggerHttpService.prependSlashIfNecessary(basePath)))
    .info(info)

  swaggerConfig.setComponents(components.get)
}
