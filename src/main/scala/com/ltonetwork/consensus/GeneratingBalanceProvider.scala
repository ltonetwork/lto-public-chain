package com.ltonetwork.consensus

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state.{Blockchain, ByteStr}
import com.ltonetwork.account.Address
import com.ltonetwork.block.Block
import com.ltonetwork.block.Block.BlockId

object GeneratingBalanceProvider {
  private val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  private val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L
  private val FirstDepth                                 = 50
  private val SecondDepth                                = 1000

  def isMiningAllowed(blockchain: Blockchain, height: Int, effectiveBalance: Long): Boolean = {
    val activated = blockchain.activatedFeatures.get(BlockchainFeatures.DUMMY_FOR_TESTS_SmallerMinimalGeneratingBalance.id).exists(height >= _)
    (!activated && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) || (activated && effectiveBalance >= MinimalEffectiveBalanceForGenerator2)
  }

  def isEffectiveBalanceValid(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, block: Block, effectiveBalance: Long): Boolean =
    (effectiveBalance >= MinimalEffectiveBalanceForGenerator1) ||
      blockchain.activatedFeatures
        .get(BlockchainFeatures.DUMMY_FOR_TESTS_SmallerMinimalGeneratingBalance.id)
        .exists(height >= _) && effectiveBalance >= MinimalEffectiveBalanceForGenerator2

  def balance(blockchain: Blockchain, fs: FunctionalitySettings, account: Address, blockId: BlockId = ByteStr.empty): Long = {
    val height =
      if (blockId == ByteStr.empty) blockchain.height
      else blockchain.heightOf(blockId).getOrElse(throw new IllegalArgumentException(s"Invalid block ref: $blockId"))
    val depth = SecondDepth
    blockchain.effectiveBalance(account, depth, blockId)
  }
}
