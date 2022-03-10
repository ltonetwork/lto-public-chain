package com.ltonetwork.block
import cats.Monoid
import com.ltonetwork.block.Block.CurrentBlockFeePart
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.features.FeatureProvider._
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state.{Blockchain, Portfolio}
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.utils._

object BlockRewardCalculator {
  val feeBurnAmt: Long = 0.1.lto
  val feeBurnPct       = 0.5

  def blockReward(settings: FunctionalitySettings, bc: Blockchain): Portfolio =
    Portfolio(balance = bc.featureActivationHeight(BlockchainFeatures.TokenomicsRedefined)
      .map(height => settings.miningReward + settings.miningRewardBonus * Math.max(height - bc.height + settings.miningRewardBonusPeriod, 0))
      .getOrElse(0))

  def rewardedFee(bc: Blockchain, tx: Transaction): Portfolio = {
    if (bc.isFeatureActivated(BlockchainFeatures.TokenomicsRedefined, bc.height))
      Portfolio(balance = (tx.fee * (1 - feeBurnPct)).toLong)
    else if (bc.isFeatureActivated(BlockchainFeatures.BurnFeeture, bc.height))
      Portfolio(balance = Math.max(0, tx.fee - feeBurnAmt))
    else
      Portfolio(balance = tx.fee)
  }

  def burnedFee(bc: Blockchain, tx: Transaction): Portfolio =
    Portfolio(balance = tx.fee - rewardedFee(bc, tx).balance)

  def prevBlockFeeDistr(bc: Blockchain, block: Block): Portfolio =
    Monoid[Portfolio].combineAll(block.transactionData.map { tx =>
      val fees = BlockRewardCalculator.rewardedFee(bc, tx)
      fees.minus(fees.multiply(CurrentBlockFeePart))
    })

  def curBlockFeeDist(bc: Blockchain, block: Block): Portfolio =
    Monoid[Portfolio].combineAll(block.transactionData.map { tx =>
      BlockRewardCalculator.rewardedFee(bc, tx).multiply(CurrentBlockFeePart)
    })
}
