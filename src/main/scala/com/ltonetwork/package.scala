package com

import com.ltonetwork.block.Block
import com.ltonetwork.settings.LtoSettings
import com.ltonetwork.state.NG
import com.ltonetwork.transaction.BlockchainUpdater
import com.ltonetwork.utils.ScorexLogging

package object ltonetwork extends ScorexLogging {
  def checkGenesis(settings: LtoSettings, blockchainUpdater: BlockchainUpdater with NG): Unit = if (blockchainUpdater.isEmpty) {
    Block.genesis(settings.blockchainSettings.genesisSettings).flatMap(blockchainUpdater.processBlock).left.foreach { value =>
      log.error(value.toString)
      com.ltonetwork.utils.forceStopApplication()
    }
    log.info(s"Genesis block ${blockchainUpdater.blockHeaderAndSize(1).get._1} has been added to the state")
  }
}
