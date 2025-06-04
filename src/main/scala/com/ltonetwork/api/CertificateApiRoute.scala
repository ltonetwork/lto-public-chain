package com.ltonetwork.api

import akka.http.scaladsl.model.{HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Route
import com.ltonetwork.account.Address
import com.ltonetwork.settings.RestAPISettings
import com.ltonetwork.state.Blockchain
import com.ltonetwork.utils.{Base64, Time}
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.Schema
import io.swagger.v3.oas.annotations.tags.Tag
import io.swagger.v3.oas.annotations.{Operation, Parameter, Parameters}
import jakarta.ws.rs.{GET, Path}
import play.api.libs.json._

import java.security.cert.{CertPathValidator, CertPathValidatorException, CertificateFactory, PKIXParameters, TrustAnchor, X509Certificate}
import java.security.KeyStore
import scala.collection.JavaConverters._
import akka.http.scaladsl.model.{ContentType, MediaType}

@Path("/certificates")
@Tag(name = "certificates")
case class CertificateApiRoute(settings: RestAPISettings, blockchain: Blockchain, time: Time) extends ApiRoute {
  private val `application/pkix-cert`: ContentType.Binary =
    ContentType(MediaType.applicationBinary("pkix-cert", MediaType.NotCompressible))

  override lazy val route: Route = pathPrefix("certificates") {
    certificateStructured ~ certificateRaw
  }

  @GET
  @Path("/{address}")
  @Operation(summary = "Get parsed certificate info")
  @Parameters(Array(
    new Parameter(
      name = "address",
      description = "Wallet address",
      required = true,
      schema = new Schema(implementation = classOf[String]),
      in = ParameterIn.PATH
    )
  ))
  def certificateStructured: Route = (path(Segment) & get) { addressStr =>
    Address.fromString(addressStr) match {
      case Left(err) => complete(ApiError.fromValidationError(err))
      case Right(address) =>
        blockchain.certificate(address) match {
          case None => complete(StatusCodes.NotFound -> Json.obj("error" -> "Certificate not found"))
          case Some(certBytes) =>
            try {
              val factory = CertificateFactory.getInstance("X.509")
              val cert = factory.generateCertificate(new java.io.ByteArrayInputStream(certBytes)).asInstanceOf[X509Certificate]

              val subject = cert.getSubjectX500Principal.getName("RFC1779")
              val issuer = cert.getIssuerX500Principal.getName("RFC1779")
              val notBefore = cert.getNotBefore.getTime
              val notAfter = cert.getNotAfter.getTime
              val now = time.correctedTime()

              val status = determineStatus(cert, now)
              val json = Json.obj(
                "subject" -> subject,
                "issuer" -> issuer,
                "notBefore" -> notBefore,
                "notAfter" -> notAfter,
                "status" -> status,
                "certificate" -> s"-----BEGIN CERTIFICATE-----\n${Base64.encode(certBytes)}\n-----END CERTIFICATE-----"
              )

              complete(json)
            } catch {
              case _: Throwable => complete(StatusCodes.InternalServerError -> Json.obj("error" -> "Invalid certificate"))
            }
        }
    }
  }

  @GET
  @Path("/{address}/raw")
  @Operation(summary = "Get raw certificate (DER encoded)")
  @Parameters(Array(
    new Parameter(
      name = "address",
      description = "Wallet address",
      required = true,
      schema = new Schema(implementation = classOf[String]),
      in = ParameterIn.PATH
    )
  ))
  def certificateRaw: Route = (path(Segment / "raw") & get) { addressStr =>
    Address.fromString(addressStr) match {
      case Left(err) => complete(ApiError.fromValidationError(err))
      case Right(address) =>
        blockchain.certificate(address) match {
          case None => complete(StatusCodes.NotFound)
          case Some(certBytes) =>
            complete(HttpEntity(`application/pkix-cert`, certBytes))
        }
    }
  }

  private def determineStatus(cert: X509Certificate, now: Long): String = {
    try {
      if (now < cert.getNotBefore.getTime || now > cert.getNotAfter.getTime) {
        "expired"
      } else {
        val trustAnchors = loadTrustAnchors()
        val params = new PKIXParameters(trustAnchors.asJava)
        params.setRevocationEnabled(true)

        val factory = CertificateFactory.getInstance("X.509")
        val certPath = factory.generateCertPath(List(cert).asJava)

        val validator = CertPathValidator.getInstance("PKIX")
        validator.validate(certPath, params)

        "valid"
      }
    } catch {
      case e: CertPathValidatorException if e.getReason == CertPathValidatorException.BasicReason.REVOKED => "revoked"
      case _: CertPathValidatorException => "untrusted"
      case _: Throwable => "invalid"
    }
  }

  private def loadTrustAnchors(): Set[TrustAnchor] = {
    val javaHome = System.getProperty("java.home")
    val cacertsPath = s"$javaHome/lib/security/cacerts"
    val is = new java.io.FileInputStream(cacertsPath)

    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    ks.load(is, "changeit".toCharArray)

    ks.aliases().asScala
      .filter(ks.isCertificateEntry)
      .map(alias => new TrustAnchor(ks.getCertificate(alias).asInstanceOf[X509Certificate], null))
      .toSet
  }
}
