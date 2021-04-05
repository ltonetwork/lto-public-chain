package com.ltonetwork.state.diffs

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.settings.{FunctionalitySettings, TestFunctionalitySettings}

package object smart {
  val smartEnabledFS: FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))
}
