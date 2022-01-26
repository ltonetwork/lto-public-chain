package com.ltonetwork.mining
import com.ltonetwork.block.Block
import com.ltonetwork.consensus.PoSSelector
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.fee.{FeeCalculator, FeeVoteStatus}
import com.ltonetwork.history.{CheckpointServiceImpl, StorageFactory}
import com.ltonetwork.settings.{BlockchainSettings, FeesSettings, FunctionalitySettings, LtoSettings}
import com.ltonetwork.state._
import com.ltonetwork.transaction.BlockchainUpdater
import com.ltonetwork.utils._
import com.ltonetwork.utx.{UtxPool, UtxPoolImpl}
import com.ltonetwork.{TestHelpers, TestSchedulerService, TestTime, TestWallet, TransactionGen, WithDB}
import com.typesafe.config.ConfigFactory
import io.netty.channel.group.ChannelGroup
import org.scalamock.scalatest.MockFactory
import org.scalatest.propspec.AnyPropSpec

import scala.concurrent.duration._

class MinerSpecification extends AnyPropSpec with MockFactory with TransactionGen with WithDB with TestWallet {
  private val account = testWallet.privateKeyAccounts.head

  private def init: (BlockchainUpdater with NG, UtxPool, TestSchedulerService, MinerOptions, MinerImpl) = {
    val allChannels = mock[ChannelGroup]
    (allChannels.size _).expects().returns(1)

    val time = new TestTime()

    val origSettings = LtoSettings.fromConfig(ConfigFactory.load())
    val genesisSettings = TestHelpers.genesisSettings(Map(account.toAddress -> 500000000.lto), time.t)
      .copy(averageBlockDelay = 1.second)
    val functionalitySettings = FunctionalitySettings.TESTNET.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.MinimalGeneratingBalance.id -> 0,
        BlockchainFeatures.SmartAccounts.id -> 0,
        BlockchainFeatures.TokenomicsRedefined.id -> 0,
      )
    )
    val settings = origSettings.copy(
      blockchainSettings = BlockchainSettings('T', functionalitySettings, genesisSettings),
      featuresSettings = origSettings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false)
    )

    val blockchain = StorageFactory(settings, db, time)
    blockchain.processBlock(Block.genesis(genesisSettings).explicitGet()).explicitGet()

    val utx = new UtxPoolImpl(
      time,
      blockchain,
      new FeeCalculator(FeesSettings.empty, blockchain),
      functionalitySettings,
      settings.utxSettings
    )

    val scheduler = TestSchedulerService(time)
    val options = new MinerOptions

    val miner = new MinerImpl(
      allChannels,
      blockchain,
      new CheckpointServiceImpl(db, settings.checkpointsSettings),
      settings,
      time,
      utx,
      testWallet,
      new PoSSelector(blockchain, settings.blockchainSettings),
      scheduler,
      scheduler,
      options
    )

    (blockchain, utx, scheduler, options, miner)
  }

  property("mine an empty block") {
    val (blockchain, _, scheduler, _, miner) = init

    miner.scheduleMining()
    assert(miner.debugState == MinerDebugInfo.MiningBlocks)
    assert(blockchain.height == 1)

    scheduler.tick(100.seconds)
    assert(blockchain.height == 2)

    val block = blockchain.lastBlock.get
    assert(block.version == 4)
    assert(block.signerData.generator.toAddress == account.toAddress)
    assert(block.transactionData.isEmpty)
    assert(block.feeVote == FeeVoteStatus.Remain.vote)
  }

  property("mine a block with fee voting") {
    val (blockchain, _, scheduler, options, miner) = init

    options.feeVote = FeeVoteStatus.Increase

    miner.scheduleMining()

    scheduler.tick(100.seconds)
    assert(blockchain.height == 2)

    val block = blockchain.lastBlock.get
    assert(block.version == 4)
    assert(block.feeVote == FeeVoteStatus.Increase.vote)
  }
}
