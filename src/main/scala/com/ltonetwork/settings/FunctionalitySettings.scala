package com.ltonetwork.settings

import com.ltonetwork.features.BlockchainFeatures

case class FunctionalitySettings(featureCheckBlocksPeriod: Int,
                                 blocksForFeatureActivation: Int,
                                 preActivatedFeatures: Map[Short, Int],
                                 doubleFeaturesPeriodsAfterHeight: Int,
                                 feeVoteBlocksPeriod: Int) {

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

  val enabledFeatures: Map[Short, Int] = List(
    BlockchainFeatures.MinimalGeneratingBalance,
  ).map(_.id -> 0).toMap

  val MAINNET: FunctionalitySettings = apply(
    featureCheckBlocksPeriod = 5000,
    blocksForFeatureActivation = 4000,
    preActivatedFeatures = enabledFeatures,
    doubleFeaturesPeriodsAfterHeight = -1,
    feeVoteBlocksPeriod = 1000
  )

  val TESTNET: FunctionalitySettings = apply(
    featureCheckBlocksPeriod = 3000,
    blocksForFeatureActivation = 2000,
    preActivatedFeatures = enabledFeatures,
    doubleFeaturesPeriodsAfterHeight = -1,
    feeVoteBlocksPeriod = 1000
  )

  val configPath = "lto.blockchain.custom.functionality"
}
