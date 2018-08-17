package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import scala.concurrent.duration._

case class FunctionalitySettings(featureCheckBlocksPeriod: Int,
                                 blocksForFeatureActivation: Int,
                                 allowTemporaryNegativeUntil: Long,
                                 requireSortedTransactionsAfter: Long,
                                 generationBalanceDepthFrom50To1000AfterHeight: Int,
                                 minimalGeneratingBalanceAfter: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Int,
                                 blockVersion3AfterHeight: Int,
                                 preActivatedFeatures: Map[Short, Int],
                                 doubleFeaturesPeriodsAfterHeight: Int) {
  val dontRequireSortedTransactionsAfter: Int    = blockVersion3AfterHeight
  val allowLeasedBalanceTransferUntilHeight: Int = blockVersion3AfterHeight

  require(featureCheckBlocksPeriod > 0, "featureCheckBlocksPeriod must be greater than 0")
  require(
    (blocksForFeatureActivation > 0) && (blocksForFeatureActivation <= featureCheckBlocksPeriod),
    s"blocksForFeatureActivation must be in range 1 to $featureCheckBlocksPeriod"
  )

  def activationWindowSize(height: Int): Int =
    featureCheckBlocksPeriod * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def activationWindow(height: Int): Range =
    if (height < 1) Range(0, 0)
    else {
      val ws = activationWindowSize(height)
      Range.inclusive((height - 1) / ws * ws + 1, ((height - 1) / ws + 1) * ws)
    }

  def blocksForFeatureActivation(height: Int): Int =
    blocksForFeatureActivation * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def generatingBalanceDepth(height: Int): Int =
    if (height >= generationBalanceDepthFrom50To1000AfterHeight) 1000 else 50
}

object FunctionalitySettings {

  val enabledFeatures = List(
    BlockchainFeatures.SmallerMinimalGeneratingBalance,
    BlockchainFeatures.NG,
    BlockchainFeatures.MassTransfer,
    BlockchainFeatures.DataTransaction,
    BlockchainFeatures.FairPoS
  ).map(_.id -> 0).toMap

  val MAINNET = apply(
    featureCheckBlocksPeriod = 5000,
    blocksForFeatureActivation = 4000,
    allowTemporaryNegativeUntil = 0,
    requireSortedTransactionsAfter = Long.MaxValue,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0,
    allowTransactionsFromFutureUntil = 0,
    allowUnissuedAssetsUntil = 0,
    allowInvalidReissueInSameBlockUntilTimestamp = 0,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0,
    resetEffectiveBalancesAtHeight = -1,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = enabledFeatures,
    doubleFeaturesPeriodsAfterHeight = -1
  )

  val TESTNET = apply(
    featureCheckBlocksPeriod = 3000,
    blocksForFeatureActivation = 2000,
    allowTemporaryNegativeUntil = 0,
    requireSortedTransactionsAfter = Long.MaxValue,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0,
    allowTransactionsFromFutureUntil = 0,
    allowUnissuedAssetsUntil = 0,
    allowInvalidReissueInSameBlockUntilTimestamp = 0,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0,
    resetEffectiveBalancesAtHeight = -1,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = enabledFeatures,
    doubleFeaturesPeriodsAfterHeight = -1
  )

  val configPath = "waves.blockchain.custom.functionality"
}

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(blockTimestamp: Long,
                           timestamp: Long,
                           initialBalance: Long,
                           signature: Option[ByteStr],
                           transactions: Seq[GenesisTransactionSettings],
                           initialBaseTarget: Long,
                           averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET = GenesisSettings(
    1460678400000L,
    1465742577614L,
    Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2").toOption,
    List(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", Constants.UnitsInWave * Constants.TotalWaves - 5 * Constants.UnitsInWave),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", Constants.UnitsInWave),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", Constants.UnitsInWave),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", Constants.UnitsInWave),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", Constants.UnitsInWave),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", Constants.UnitsInWave)
    ),
    153722867L,
    60.seconds
  )

//  val TESTNET = GenesisSettings(
//    1533848511587L,
//    1533848511587L,
//    Constants.UnitsInWave * Constants.TotalWaves,
//    ByteStr.decodeBase58("4vn1hKdNsw431aJ4iuVNtUJeS2Z8QoojagTTevSqfuD5GvGuWTKQ9zVVdjr5G5dffMg8XYWEa7GxV1KQ2SU5ZNUW").toOption,
//    List(
//      GenesisTransactionSettings("3N5g7aNStjn8SBDPeyGoNR1CZLnCTmQkTN2", (Constants.UnitsInWave * Constants.TotalWaves * 0.1).toLong),
//      GenesisTransactionSettings("3N8cMFardfMUN5n45eneXEKAK4Hpi9Zfzpz", (Constants.UnitsInWave * Constants.TotalWaves * 0.6).toLong),
//      GenesisTransactionSettings("3N1WkBkDDWbgJVJxraEqkQ1aafwfwLabvLr", (Constants.UnitsInWave * Constants.TotalWaves * 0.3).toLong),
//    ),
//    100,
//    60.seconds
//  )

  val TESTNET = GenesisSettings(
    1534497076380L,
    1534497076380L,
    Constants.UnitsInLTO * Constants.TotalLTO,
    ByteStr.decodeBase58("47pP5r1Kh159XmxcfG2eQVj6dKNhub3mvGgpJovcw7EcZyJswFLYyKGYNV21BGJ8pwkajA75ZLMWFBdv3BzMRMk").toOption,
    List(
      GenesisTransactionSettings("3N6mZMgGqYn9EVAR2Vbf637iej4fFipECq8", (Constants.UnitsInLTO * Constants.TotalLTO * 0.01).toLong),
      GenesisTransactionSettings("3N51gbw5W3xvSkcAXtLnXc3SQh2m9e6TBcy", (Constants.UnitsInLTO * Constants.TotalLTO * 0.01).toLong),
      GenesisTransactionSettings("3NAxYD4nFbYqHo8gz9Hsfj13s283xNYvGNi", (Constants.UnitsInLTO * Constants.TotalLTO * 0.9).toLong),
      GenesisTransactionSettings("3Mv7ajrPLKewkBNqfxwRZoRwW6fziehp7dQ", (Constants.UnitsInLTO * Constants.TotalLTO * 0.01).toLong),
      GenesisTransactionSettings("3NARPnCPG4egZbFUQENZ6VDojQqMCpGEG9i", Constants.UnitsInLTO * (Constants.TotalLTO * 0.07).round),
    ),
    100,
    60.seconds
  )
}

case class BlockchainSettings(addressSchemeCharacter: Char, functionalitySettings: FunctionalitySettings, genesisSettings: GenesisSettings)

object BlockchainType extends Enumeration {
  val TESTNET = Value("TESTNET")
  val MAINNET = Value("MAINNET")
  val CUSTOM  = Value("CUSTOM")
}

object BlockchainSettings {
  val configPath: String = "waves.blockchain"

  def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value](s"$configPath.type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('L', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"$configPath.custom.address-scheme-character").charAt(0)
        val functionalitySettings  = config.as[FunctionalitySettings]("waves.blockchain.custom.functionality")
        val genesisSettings        = config.as[GenesisSettings]("waves.blockchain.custom.genesis")
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings
    )
  }
}
