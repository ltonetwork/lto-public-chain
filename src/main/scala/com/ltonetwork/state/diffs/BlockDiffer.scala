package com.ltonetwork.state.diffs

import cats.Monoid
import cats.implicits._
import com.ltonetwork.account.Address
import com.ltonetwork.block.Block.OpenerBlockFeePart
import com.ltonetwork.block.{Block, BlockRewardCalculator, MicroBlock}
import com.ltonetwork.metrics.Instrumented
import com.ltonetwork.mining.MiningConstraint
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state._
import com.ltonetwork.state.reader.CompositeBlockchain.composite
import com.ltonetwork.transaction.{Transaction, ValidationError}
import com.ltonetwork.utils._

object BlockDiffer extends ScorexLogging with Instrumented {

  val feeBurnAmt: Long = 0.1.lto
  val feeBurnPct       = 0.5

  def fromBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                maybePrevBlock: Option[Block],
                                                block: Block,
                                                constraint: Constraint,
                                                verify: Boolean = true): Either[ValidationError, (Diff, Long, Constraint)] = {
    val stateHeight = blockchain.height
    val blockGenerator = block.signerData.generator

    val miningReward = BlockRewardCalculator.miningReward(settings, blockchain)
    val initDiff = Diff.empty.copy(
      portfolios = Map(blockGenerator.toAddress -> Monoid[Portfolio].combine(
        maybePrevBlock.map(b => BlockRewardCalculator.closerBlockFee(blockchain, stateHeight, b)).orEmpty, // NG reward for closing block
        miningReward,
      )),
      burned = -1 * miningReward.balance
    )

    for {
      _ <- if (verify) block.signaturesValid() else Right(())
      r <- apply(
        settings,
        blockchain,
        constraint,
        initDiff,
        maybePrevBlock.map(_.timestamp),
        blockGenerator,
        block.timestamp,
        block.transactionData,
        stateHeight + 1
      )
    } yield r
  }

  def fromMicroBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                     blockchain: Blockchain,
                                                     prevBlockTimestamp: Option[Long],
                                                     micro: MicroBlock,
                                                     timestamp: Long,
                                                     constraint: Constraint
  ): Either[ValidationError, (Diff, Long, Constraint)] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- micro.signaturesValid()
      r <- apply(
        settings,
        blockchain,
        constraint,
        Diff.empty,
        prevBlockTimestamp,
        micro.sender,
        timestamp,
        micro.transactionData,
        blockchain.height
      )
    } yield r
  }

  private def updateConstraint[Constraint <: MiningConstraint](constraint: Constraint, blockchain: Blockchain, tx: Transaction): Constraint =
    constraint.put(blockchain, tx).asInstanceOf[Constraint]

  private def apply[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                    blockchain: Blockchain,
                                                    initConstraint: Constraint,
                                                    initDiff: Diff,
                                                    prevBlockTimestamp: Option[Long],
                                                    blockGenerator: Address,
                                                    timestamp: Long,
                                                    txs: Seq[Transaction],
                                                    currentBlockHeight: Int
  ): Either[ValidationError, (Diff, Long, Constraint)] = {
    val txDiffer = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight) _

    txs
      .foldLeft((initDiff, 0L, initConstraint).asRight[ValidationError]) {
        case (r @ Left(_), _) => r
        case (Right((currDiff, _, currConstraint)), tx) =>
          val updatedBlockchain = composite(blockchain, currDiff, 0)
          val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
          if (updatedConstraint.isOverfilled)
            Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
          else
            txDiffer(updatedBlockchain, tx).map { newDiff =>
              val updatedDiff  = currDiff.combine(newDiff)
              val curBlockFees = BlockRewardCalculator.rewardedFee(blockchain, currentBlockHeight, tx).multiply(OpenerBlockFeePart)
              val burnedFees   = BlockRewardCalculator.burnedFee(blockchain, currentBlockHeight, tx)
              val diff         = updatedDiff.combine(Diff.empty.copy(
                portfolios = Map(blockGenerator -> curBlockFees),
                burned = burnedFees,
              ))
              (diff, 0L, updatedConstraint)
            }
      }
  }
}
