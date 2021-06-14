package com.ltonetwork.settings

import com.typesafe.config.Config
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.state.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import scala.concurrent.duration._

case class FunctionalitySettings(featureCheckBlocksPeriod: Int,
                                 blocksForFeatureActivation: Int,
                                 preActivatedFeatures: Map[Short, Int],
                                 doubleFeaturesPeriodsAfterHeight: Int) {

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

  def generatingBalanceDepth(height: Int): Int = 1000
}

object FunctionalitySettings {

  val enabledFeatures = List(
    BlockchainFeatures.SmallerMinimalGeneratingBalance,
  ).map(_.id -> 0).toMap

  val MAINNET = apply(
    featureCheckBlocksPeriod = 5000,
    blocksForFeatureActivation = 4000,
    preActivatedFeatures = enabledFeatures,
    doubleFeaturesPeriodsAfterHeight = -1
  )

  val TESTNET = apply(
    featureCheckBlocksPeriod = 3000,
    blocksForFeatureActivation = 2000,
    preActivatedFeatures = enabledFeatures,
    doubleFeaturesPeriodsAfterHeight = -1
  )

  val configPath = "lto.blockchain.custom.functionality"
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
    1547303338475L,
    1547303338475L,
    Constants.UnitsInLTO * Constants.TotalLTOMain,
    ByteStr.decodeBase58("3xfb8SHvXK1eUT73kqq89Ayvt2w5ivy9CM2HHVYZ6H5zjgmbukadkFhis74vXsfak9YjwUCBMQUKsKmiRPmAWaB3").toOption,
    List(
      GenesisTransactionSettings("3JpzrZcSDhzRVeENoqqB98c6hTNg7WJaBKt", Constants.UnitsInLTO * 1000L),
      GenesisTransactionSettings("3JqBJaDet2MWisRPNLtN5snCxkGHtKaLRHv", Constants.UnitsInLTO * 1000L),
      GenesisTransactionSettings("3JygettiPvCrb7rSoWDzRHbBWKdMva2d5tu", Constants.UnitsInLTO * 499997000L),
      GenesisTransactionSettings("3JyxAP1fpeYXv77FzxihgLsDVMccwLE64rd", Constants.UnitsInLTO * 1000L)
    ),
    153722867L,
    60.seconds
  )

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
  val configPath: String = "lto.blockchain"

  def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value](s"$configPath.type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('L', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"$configPath.custom.address-scheme-character").charAt(0)
        val functionalitySettings  = config.as[FunctionalitySettings]("lto.blockchain.custom.functionality")
        val genesisSettings        = config.as[GenesisSettings]("lto.blockchain.custom.genesis")
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings
    )
  }
}
