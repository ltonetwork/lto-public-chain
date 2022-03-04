package com.ltonetwork.api

import akka.http.scaladsl.server.Route
import com.ltonetwork.network.{PeerDatabase, PeerInfo}
import com.ltonetwork.settings.RestAPISettings
import io.netty.channel.Channel
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.responses.{ApiResponse, ApiResponses}
import io.swagger.v3.oas.annotations.security.SecurityRequirement
import io.swagger.v3.oas.annotations.tags.Tag
import jakarta.ws.rs.{GET, POST, Path}
import play.api.libs.json._

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ConcurrentMap
import java.util.stream.Collectors
import scala.collection.JavaConverters._

@Path("/peers")
@Tag(name = "peers")
case class PeersApiRoute(settings: RestAPISettings,
                         connectToPeer: InetSocketAddress => Unit,
                         peerDatabase: PeerDatabase,
                         establishedConnections: ConcurrentMap[Channel, PeerInfo])
    extends ApiRoute {

  import PeersApiRoute._

  override lazy val route =
    pathPrefix("peers") {
      allPeers ~ connectedPeers ~ blacklistedPeers ~ suspendedPeers ~ connect ~ clearBlacklist
    }

  @GET
  @Path("/all")
  @Operation(
    summary = "Peer list"
  )
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "Json with peer list or error")
    ))
  def allPeers: Route = (path("all") & get) {
    complete(
      Json.obj(
        "peers" ->
          JsArray(peerDatabase.knownPeers
            .take(MaxPeersInResponse)
            .map {
              case (address, timestamp) =>
                Json.obj(
                  "address"  -> address.toString,
                  "lastSeen" -> timestamp
                )
            }
            .toList)))
  }

  @GET
  @Path("/connected")
  @Operation(
    summary = "Connected peers list"
  )
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "Json with connected peers or error")
    ))
  def connectedPeers: Route = (path("connected") & get) {
    val peers = establishedConnections
      .values()
      .stream()
      .map[JsValue](pi =>
        Json.obj(
          "address"            -> pi.remoteAddress.toString,
          "declaredAddress"    -> pi.declaredAddress.fold("N/A")(_.toString),
          "peerName"           -> pi.nodeName,
          "peerNonce"          -> pi.nodeNonce,
          "applicationName"    -> pi.applicationName,
          "applicationVersion" -> s"${pi.applicationVersion._1}.${pi.applicationVersion._2}.${pi.applicationVersion._3}"
      ))
      .collect(Collectors.toList())
      .asScala

    complete(Json.obj("peers" -> JsArray(peers)))
  }

  @POST
  @Path("/connect")
  @Operation(
    summary = "Connect to peer"
  )
  @SecurityRequirement(name = "bearerAuth")
  @RequestBody(
    description = "Json with data",
    content = Array(
      new Content(
        schema = new Schema(implementation = classOf[String]),
        examples = Array(
          new ExampleObject(
            value = "{\n\t\"host\":\"127.0.0.1\",\n\t\"port\":\"9084\"\n}"
          ))
      )),
    required = true
  )
  def connect: Route = (path("connect") & post & withAuth) {
    json[ConnectReq] { req =>
      val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(req.host), req.port)
      connectToPeer(add)

      Json.obj("hostname" -> add.getHostName, "status" -> "Trying to connect")
    }
  }

  @GET
  @Path("/blacklisted")
  @Operation(
    summary = "Blacklisted peers list"
  )
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "Json with blacklisted peers or error")
    ))
  def blacklistedPeers: Route = (path("blacklisted") & get) {
    complete(
      JsArray(
        peerDatabase.detailedBlacklist
          .take(MaxPeersInResponse)
          .map { case (h, (t, r)) => Json.obj("hostname" -> h.toString, "timestamp" -> t, "reason" -> r) }
          .toList))
  }

  @GET
  @Path("/suspended")
  @Operation(
    summary = "Suspended peers list"
  )
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "JSON with suspended peers or error")
    ))
  def suspendedPeers: Route = (path("suspended") & get) {
    complete(
      JsArray(
        peerDatabase.detailedSuspended.take(MaxPeersInResponse).map { case (h, t) => Json.obj("hostname" -> h.toString, "timestamp" -> t) }.toList))
  }

  @POST
  @Path("/clearblacklist")
  @Operation(
    summary = "Remove all blacklisted peers"
  )
  @SecurityRequirement(name = "bearerAuth")
  @ApiResponses(
    Array(
      new ApiResponse(responseCode = "200", description = "200")
    ))
  def clearBlacklist: Route = (path("clearblacklist") & post & withAuth) {
    peerDatabase.clearBlacklist()
    complete(Json.obj("result" -> "blacklist cleared"))
  }
}

object PeersApiRoute {
  val MaxPeersInResponse = 1000

  case class ConnectReq(host: String, port: Int)

  implicit val connectFormat: Format[ConnectReq] = Json.format
}
