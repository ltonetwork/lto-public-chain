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

  def miningReward(settings: FunctionalitySettings, bc: Blockchain, height: Int): Portfolio =
    Portfolio(balance = bc.featureActivationHeight(BlockchainFeatures.Juicy).map(activationHeight => settings.miningReward
        + settings.miningRewardBonus * Math.max(activationHeight - height + settings.miningRewardBonusPeriod, 0))
      .getOrElse(0))
  def miningReward(settings: FunctionalitySettings, bc: Blockchain): Portfolio = miningReward(settings, bc, bc.height)

  def rewardedFee(bc: Blockchain, tx: Transaction): Portfolio =
    Portfolio(balance = tx.fee - burnedFee(bc, tx))

  def burnedFee(bc: Blockchain, tx: Transaction): Long = {
    if (bc.isFeatureActivated(BlockchainFeatures.Juicy, bc.height))
      (tx.fee * feeBurnPct).toLong
    else if (bc.isFeatureActivated(BlockchainFeatures.BurnFeeture, bc.height))
      Math.min(tx.fee, feeBurnAmt)
    else
      0L
  }

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
