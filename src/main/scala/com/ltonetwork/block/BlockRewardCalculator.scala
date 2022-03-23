package com.ltonetwork.block
import cats.Monoid
import com.ltonetwork.block.Block.CurrentBlockFeePart
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.features.FeatureProvider._
import com.ltonetwork.fee.FeeCalculator
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state.{Blockchain, Portfolio}
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.utils._

object BlockRewardCalculator {
  val feeBurnAmt: Long = 0.1.lto
  val feeBurnPct       = 0.5

  def miningReward(settings: FunctionalitySettings, bc: Blockchain, height: Int): Portfolio =
    Portfolio(balance = bc.featureActivationHeight(BlockchainFeatures.Juicy).map(activationHeight => settings.miningReward
        + settings.miningRewardBonus * Math.max(activationHeight - height + settings.miningRewardBonusPeriod, 0))
      .getOrElse(0))
  def miningReward(settings: FunctionalitySettings, bc: Blockchain): Portfolio = miningReward(settings, bc, bc.height)

  def rewardedFee(bc: Blockchain, height: Int, tx: Transaction): Portfolio = {
    Portfolio(balance = {
      if (bc.isFeatureActivated(BlockchainFeatures.Juicy, height))
        (FeeCalculator(bc).fee(height, tx) * (1 - feeBurnPct)).toLong
      else if (bc.isFeatureActivated(BlockchainFeatures.BurnFeeture, height))
        Math.max(tx.fee - feeBurnAmt, 0)
      else
        tx.fee
    })
  }

  def burnedFee(bc: Blockchain, height: Int, tx: Transaction): Long =
    if (bc.isFeatureActivated(BlockchainFeatures.Juicy, height))
      (FeeCalculator(bc).fee(height, tx) * feeBurnPct).toLong
    else if (bc.isFeatureActivated(BlockchainFeatures.BurnFeeture, height))
      Math.min(tx.fee, feeBurnAmt)
    else
      0L

  def blockFee(bc: Blockchain, height: Int, block: Block): Portfolio =
    Monoid[Portfolio].combineAll(block.transactionData.map { tx =>
      Portfolio(balance = FeeCalculator(bc).fee(height, tx))
    })

  def blockBurnedFee(bc: Blockchain, height: Int, block: Block): Long =
      block.transactionData.map(burnedFee(bc, height, _)).sum

  def openerBlockFee(bc: Blockchain, height: Int, block: Block): Portfolio =
    Monoid[Portfolio].combineAll(block.transactionData.map { tx =>
      val fees = rewardedFee(bc, height, tx)
      fees.minus(fees.multiply(CurrentBlockFeePart))
    })

  def closerBlockFee(bc: Blockchain, height: Int, block: Block): Portfolio =
    Monoid[Portfolio].combineAll(block.transactionData.map { tx =>
      rewardedFee(bc, height, tx).multiply(CurrentBlockFeePart)
    })
}