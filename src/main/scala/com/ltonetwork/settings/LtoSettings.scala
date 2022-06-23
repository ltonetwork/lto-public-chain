package com.ltonetwork.settings

import com.ltonetwork.metrics.Metrics
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class LtoSettings(directory: String,
                       dataDirectory: String,
                       maxCacheSize: Int,
                       maxRollbackDepth: Int,
                       indexAllTransactions: Boolean,
                       networkSettings: NetworkSettings,
                       walletSettings: WalletSettings,
                       blockchainSettings: BlockchainSettings,
                       checkpointsSettings: CheckpointsSettings,
                       minerSettings: MinerSettings,
                       restAPISettings: RestAPISettings,
                       synchronizationSettings: SynchronizationSettings,
                       utxSettings: UtxSettings,
                       featuresSettings: FeaturesSettings,
                       metrics: Metrics.Settings)

object LtoSettings {

  import NetworkSettings.networkSettingsValueReader

  def fromConfig(config: Config): LtoSettings = {
    val directory               = config.as[String]("lto.directory")
    val dataDirectory           = config.as[String]("lto.data-directory")
    val maxCacheSize            = config.as[Int]("lto.max-cache-size")
    val maxRollbackDepth        = config.as[Int]("lto.max-rollback-depth")
    val indexAllTransactions    = config.as[Boolean]("lto.index-all-transactions")
    val networkSettings         = config.as[NetworkSettings]("lto.network")
    val walletSettings          = config.as[WalletSettings]("lto.wallet")
    val blockchainSettings      = BlockchainSettings.fromConfig(config)
    val checkpointsSettings     = CheckpointsSettings.fromConfig(config)
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
      indexAllTransactions,
      networkSettings,
      walletSettings,
      blockchainSettings,
      checkpointsSettings,
      minerSettings,
      restAPISettings,
      synchronizationSettings,
      utxSettings,
      featuresSettings,
      metrics
    )
  }
}
