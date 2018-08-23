package one

import one.legalthings.block.Block
import one.legalthings.settings.WavesSettings
import one.legalthings.state.NG
import one.legalthings.transaction.BlockchainUpdater
import one.legalthings.utils.ScorexLogging

package object legalthings extends ScorexLogging {
  def checkGenesis(settings: WavesSettings, blockchainUpdater: BlockchainUpdater with NG): Unit = if (blockchainUpdater.isEmpty) {
    Block.genesis(settings.blockchainSettings.genesisSettings).flatMap(blockchainUpdater.processBlock).left.foreach { value =>
      log.error(value.toString)
      one.legalthings.utils.forceStopApplication()
    }
    log.info(s"Genesis block ${blockchainUpdater.blockHeaderAndSize(1).get._1} has been added to the state")
  }
}
