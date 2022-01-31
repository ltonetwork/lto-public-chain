package com.ltonetwork.state.diffs

import cats.Monoid
import cats.implicits._
import cats.syntax.either.catsSyntaxEitherId
import com.ltonetwork.account.Address
import com.ltonetwork.block.Block.CurrentBlockFeePart
import com.ltonetwork.block.{Block, MicroBlock}
import com.ltonetwork.features.BlockchainFeatures
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

  def maybeBurnFee(bc: Blockchain, tx: Transaction): Portfolio = {
    import com.ltonetwork.features.FeatureProvider._
    if (bc.isFeatureActivated(BlockchainFeatures.TokenomicsRedefined, bc.height))
      Portfolio(balance = (tx.fee * (1 - feeBurnPct)).toLong, lease = LeaseBalance.empty)
    else if (bc.isFeatureActivated(BlockchainFeatures.BurnFeeture, bc.height))
      Portfolio(balance = Math.max(0, tx.fee - feeBurnAmt))
    else
      Portfolio(balance = tx.fee)
  }

  def fromBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                maybePrevBlock: Option[Block],
                                                block: Block,
                                                constraint: Constraint,
                                                verify: Boolean = true): Either[ValidationError, (Diff, Long, Constraint)] = {
    val stateHeight = blockchain.height

    lazy val prevBlockFeeDistr: Option[Portfolio] = maybePrevBlock.map(b =>
      Monoid[Portfolio].combineAll(b.transactionData.map { tx =>
        val fees = maybeBurnFee(blockchain, tx)
        fees.minus(fees.multiply(CurrentBlockFeePart))
      }))

    lazy val currentBlockFeeDistr: Option[Portfolio] = None

    for {
      _ <- block.signaturesValid()
      r <- apply(
        settings,
        blockchain,
        constraint,
        maybePrevBlock.map(_.timestamp),
        block.signerData.generator,
        prevBlockFeeDistr,
        currentBlockFeeDistr,
        block.timestamp,
        block.transactionData,
        stateHeight + 1 //,
        // verify
      )
    } yield r
  }

  def fromMicroBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                     blockchain: Blockchain,
                                                     prevBlockTimestamp: Option[Long],
                                                     micro: MicroBlock,
                                                     timestamp: Long,
                                                     constraint: Constraint
                                                     //,
                                                     // verify: Boolean = true
  ): Either[ValidationError, (Diff, Long, Constraint)] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- micro.signaturesValid()
      r <- apply(
        settings,
        blockchain,
        constraint,
        prevBlockTimestamp,
        micro.sender,
        None,
        None,
        timestamp,
        micro.transactionData,
        blockchain.height // ,
        // verify
      )
    } yield r
  }

  private def apply[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                    blockchain: Blockchain,
                                                    initConstraint: Constraint,
                                                    prevBlockTimestamp: Option[Long],
                                                    blockGenerator: Address,
                                                    prevBlockFeeDistr: Option[Portfolio],
                                                    currentBlockFeeDistr: Option[Portfolio],
                                                    timestamp: Long,
                                                    txs: Seq[Transaction],
                                                    currentBlockHeight: Int // ,
                                                    // verify: Boolean
  ): Either[ValidationError, (Diff, Long, Constraint)] = {
    def updateConstraint(constraint: Constraint, blockchain: Blockchain, tx: Transaction): Constraint =
      constraint.put(blockchain, tx).asInstanceOf[Constraint]

    val txDiffer = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight) _
    val initDiff = Diff.empty.copy(portfolios = Map(blockGenerator -> currentBlockFeeDistr.orElse(prevBlockFeeDistr).orEmpty))
    val hasNg    = currentBlockFeeDistr.isEmpty

    def calcNextBlockFee(blockchain: Blockchain, portfolio: Portfolio): (Portfolio, Long) = {
      val ngPf = if (hasNg) {
        val curPf  = portfolio.multiply(Block.CurrentBlockFeePart)
        val nextPf = portfolio.minus(curPf)
        (curPf, nextPf.balance)
      } else (portfolio, 0L)
      ngPf.copy(_2 = 0L)
    }

    txs
      .foldLeft((initDiff, 0L, initConstraint).asRight[ValidationError]) {
        case (r @ Left(_), _) => r
        case (Right((currDiff, carryFee, currConstraint)), tx) =>
          val updatedBlockchain = composite(blockchain, currDiff, 0)
          val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
          if (updatedConstraint.isOverfilled)
            Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
          else
            txDiffer(updatedBlockchain, tx).map { newDiff =>
              val updatedDiff = currDiff.combine(newDiff)
              if (hasNg) {
                val (curBlockFees, nextBlockFee) = calcNextBlockFee(updatedBlockchain, maybeBurnFee(blockchain, tx))
                val diff                         = updatedDiff.combine(Diff.empty.copy(portfolios = Map(blockGenerator -> curBlockFees)))
                (diff, carryFee + nextBlockFee, updatedConstraint)
              } else (updatedDiff, 0L, updatedConstraint)
            }
      }
  }
}
