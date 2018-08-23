package one.legalthings.it.api

import one.legalthings.it.Node
import one.legalthings.network.RawBytes
import one.legalthings.network.client.NetworkSender

import scala.concurrent.Future

object AsyncNetworkApi {

  implicit class NodeAsyncNetworkApi(n: Node) {

    import scala.concurrent.ExecutionContext.Implicits.global

    def nonce: Long = System.currentTimeMillis()

    def sendByNetwork(message: RawBytes*): Future[Unit] = {
      val sender = new NetworkSender(n.settings.blockchainSettings.addressSchemeCharacter, s"it-client-to-${n.name}", nonce)
      sender.connect(n.networkAddress).map { ch =>
        if (ch.isActive) sender.send(ch, message: _*).map(_ => sender.close()) else sender.close()
      }
    }
  }
}
