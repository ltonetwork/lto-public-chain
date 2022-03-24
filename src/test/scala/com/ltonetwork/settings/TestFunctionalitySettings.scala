package com.ltonetwork.settings

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.utils._

object TestFunctionalitySettings {
  val Enabled = FunctionalitySettings(
    featureCheckBlocksPeriod = 10000,
    blocksForFeatureActivation = 9000,
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id          -> 0,
      BlockchainFeatures.AssociationTransaction.id -> 0,
      BlockchainFeatures.SponsorshipTransaction.id -> 0,
      BlockchainFeatures.Cobalt.id                 -> 0,
      BlockchainFeatures.CobaltAlloy.id            -> 0
    ),
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    feeVoteBlocksPeriod = 1000,
    blocksForFeeChange = 600,
    miningReward = 10.lto,
    miningRewardBonus = 0.1.lto,
    miningRewardBonusPeriod = 50,
    leaseUnbondingPeriod = 20,
  )
  val Disabled = Enabled.copy(preActivatedFeatures = Map.empty)

  val Stub: FunctionalitySettings = Enabled.copy(
    featureCheckBlocksPeriod = 100,
    blocksForFeatureActivation = 90,
    feeVoteBlocksPeriod = 10
  )

  val EmptyFeaturesSettings: FeaturesSettings =
    FeaturesSettings(autoShutdownOnUnsupportedFeature = false, List.empty)
}
