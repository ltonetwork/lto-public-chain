package one.legalthings.it

import java.net.{InetSocketAddress, URL}

import com.typesafe.config.Config
import one.legalthings.account.{PrivateKeyAccount, PublicKeyAccount}
import one.legalthings.it.util.GlobalTimer
import one.legalthings.settings.WavesSettings
import one.legalthings.state.EitherExt2
import one.legalthings.utils.{Base58, LoggerFacade}
import org.asynchttpclient.Dsl.{config => clientConfig, _}
import org.asynchttpclient._
import org.slf4j.LoggerFactory

import scala.concurrent.duration.FiniteDuration

abstract class Node(config: Config) extends AutoCloseable {
  lazy val log: LoggerFacade =
    LoggerFacade(LoggerFactory.getLogger(s"${getClass.getCanonicalName}.${this.name}"))

  val settings: WavesSettings = WavesSettings.fromConfig(config)
  val client: AsyncHttpClient = asyncHttpClient(
    clientConfig()
      .setKeepAlive(false)
      .setNettyTimer(GlobalTimer.instance))

  val privateKey: PrivateKeyAccount = PrivateKeyAccount.fromSeed(config.getString("account-seed")).explicitGet()
  val publicKey: PublicKeyAccount   = PublicKeyAccount.fromBase58String(config.getString("public-key")).explicitGet()
  val address: String               = config.getString("address")

  def nodeApiEndpoint: URL
  def matcherApiEndpoint: URL
  def apiKey: String

  /** An address which can be reached from the host running IT (may not match the declared address) */
  def networkAddress: InetSocketAddress

  override def close(): Unit = client.close()
}

object Node {
  implicit class NodeExt(val n: Node) extends AnyVal {
    def name: String = n.settings.networkSettings.nodeName

    def publicKeyStr = Base58.encode(n.publicKey.publicKey)

    def fee(txTypeId: Byte, asset: String = "WAVES"): Long = n.settings.feesSettings.fees(txTypeId).find(_.asset == asset).get.fee

    def blockDelay: FiniteDuration = n.settings.blockchainSettings.genesisSettings.averageBlockDelay
  }
}
