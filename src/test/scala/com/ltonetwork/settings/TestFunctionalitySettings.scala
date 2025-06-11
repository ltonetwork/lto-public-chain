package com.ltonetwork.settings

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.utils._

object TestFunctionalitySettings {
  private val preActivatedFeatures = Map(
    BlockchainFeatures.SmartAccounts.id          -> 0,
    BlockchainFeatures.AssociationTransaction.id -> 0,
    BlockchainFeatures.SponsorshipTransaction.id -> 0,
    BlockchainFeatures.Cobalt.id                 -> 0,
    BlockchainFeatures.CobaltAlloy.id            -> 0,
    BlockchainFeatures.Titanium.id               -> 0,
    BlockchainFeatures.Palladium.id              -> 0,
  )

  val Enabled = FunctionalitySettings(
    featureCheckBlocksPeriod = 10000,
    blocksForFeatureActivation = 9000,
    preActivatedFeatures = preActivatedFeatures,
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    feeVoteBlocksPeriod = 1000,
    blocksForFeeChange = 600,
    miningReward = 10.lto,
    miningRewardBonus = 0.1.lto,
    miningRewardBonusPeriod = 50,
    leaseUnbondingPeriod = 20,
    burnAddresses = Set.empty[String]
  )
  val Disabled = Enabled.copy(preActivatedFeatures = Map.empty)

  val Juicy = Enabled.copy(
    preActivatedFeatures = preActivatedFeatures ++ Map(
      BlockchainFeatures.Juicy.id -> 0,
      BlockchainFeatures.BurnFeeture.id -> 0
    )
  )

  val Stub: FunctionalitySettings = Enabled.copy(
    featureCheckBlocksPeriod = 100,
    blocksForFeatureActivation = 90,
    feeVoteBlocksPeriod = 10
  )

  val EmptyFeaturesSettings: FeaturesSettings =
    FeaturesSettings(autoShutdownOnUnsupportedFeature = false, List.empty)
}
