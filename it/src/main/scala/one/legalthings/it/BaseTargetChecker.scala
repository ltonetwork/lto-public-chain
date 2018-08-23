package one.legalthings.it

import java.time.Instant

import com.typesafe.config.ConfigFactory.{defaultApplication, defaultReference}
import one.legalthings.consensus.PoSSelector
import one.legalthings.db.openDB
import one.legalthings.history.StorageFactory
import one.legalthings.settings._
import one.legalthings.state.{ByteStr, EitherExt2}
import net.ceedubs.ficus.Ficus._
import one.legalthings.account.PublicKeyAccount
import one.legalthings.utils.NTP
import one.legalthings.block.Block

object BaseTargetChecker {
  def main(args: Array[String]): Unit = {
    val docker = Docker(getClass)
    val sharedConfig = docker.genesisOverride
      .withFallback(docker.configTemplate)
      .withFallback(defaultApplication())
      .withFallback(defaultReference())
      .resolve()
    val settings     = WavesSettings.fromConfig(sharedConfig)
    val genesisBlock = Block.genesis(settings.blockchainSettings.genesisSettings).explicitGet()
    val db           = openDB("/tmp/tmp-db")
    val bu           = StorageFactory(settings, db, NTP)
    val pos          = new PoSSelector(bu, settings.blockchainSettings)
    bu.processBlock(genesisBlock)

    println(s"Genesis TS = ${Instant.ofEpochMilli(genesisBlock.timestamp)}")

    val m = NodeConfigs.Default.map(_.withFallback(sharedConfig)).collect {
      case cfg if cfg.as[Boolean]("waves.miner.enable") =>
        val account   = PublicKeyAccount(cfg.as[ByteStr]("public-key").arr)
        val address   = account.toAddress
        val balance   = bu.balance(address)
        val consensus = genesisBlock.consensusData
        val timeDelay = pos
          .getValidBlockDelay(bu.height, account.publicKey, consensus.baseTarget, balance)
          .explicitGet()

        f"$address: ${timeDelay * 1e-3}%10.3f s"
    }

    docker.close()

    println(m.mkString("\n"))
  }
}
