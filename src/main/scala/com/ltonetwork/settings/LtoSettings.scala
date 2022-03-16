package com.ltonetwork.settings

import com.ltonetwork.metrics.Metrics
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class LtoSettings(directory: String,
                       dataDirectory: String,
                       maxCacheSize: Int,
                       maxRollbackDepth: Int,
                       networkSettings: NetworkSettings,
                       walletSettings: WalletSettings,
                       blockchainSettings: BlockchainSettings,
                       checkpointsSettings: CheckpointsSettings,
                       feesSettings: FeesSettings,
                       minerSettings: MinerSettings,
                       restAPISettings: RestAPISettings,
                       synchronizationSettings: SynchronizationSettings,
                       utxSettings: UtxSettings,
                       featuresSettings: FeaturesSettings,
                       metrics: Metrics.Settings)

object LtoSettings {

  import NetworkSettings.networkSettingsValueReader

  val configPath: String = "lto"

  def fromConfig(config: Config): LtoSettings = {
    val directory               = config.as[String](s"$configPath.directory")
    val dataDirectory           = config.as[String](s"$configPath.data-directory")
    val maxCacheSize            = config.as[Int](s"$configPath.max-cache-size")
    val maxRollbackDepth        = config.as[Int](s"$configPath.max-rollback-depth")
    val networkSettings         = config.as[NetworkSettings]("lto.network")
    val walletSettings          = config.as[WalletSettings]("lto.wallet")
    val blockchainSettings      = BlockchainSettings.fromConfig(config)
    val checkpointsSettings     = CheckpointsSettings.fromConfig(config)
    val feesSettings            = FeesSettings.fromConfig(config)
    val minerSettings           = MinerSettings.fromConfig(config)
    val restAPISettings         = RestAPISettings.fromConfig(config)
    val synchronizationSettings = SynchronizationSettings.fromConfig(config)
    val utxSettings             = config.as[UtxSettings]("lto.utx")
    val featuresSettings        = config.as[FeaturesSettings]("lto.features")
    val metrics                 = config.as[Metrics.Settings]("metrics")

    LtoSettings(
      directory,
      dataDirectory,
      maxCacheSize,
      maxRollbackDepth,
      networkSettings,
      walletSettings,
      blockchainSettings,
      checkpointsSettings,
      feesSettings,
      minerSettings,
      restAPISettings,
      synchronizationSettings,
      utxSettings,
      featuresSettings,
      metrics
    )
  }
}
