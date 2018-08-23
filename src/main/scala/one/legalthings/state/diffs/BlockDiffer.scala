package one.legalthings.state.diffs

import cats.Monoid
import cats.implicits._
import cats.syntax.either.catsSyntaxEitherId
import one.legalthings.features.BlockchainFeatures
import one.legalthings.features.FeatureProvider._
import one.legalthings.metrics.Instrumented
import one.legalthings.mining.MiningConstraint
import one.legalthings.settings.FunctionalitySettings
import one.legalthings.state._
import one.legalthings.state.patch.{CancelAllLeases, CancelInvalidLeaseIn, CancelLeaseOverflow}
import one.legalthings.state.reader.CompositeBlockchain.composite
import one.legalthings.account.Address
import one.legalthings.utils.ScorexLogging
import one.legalthings.block.{Block, MicroBlock}
import one.legalthings.transaction.ValidationError.ActivationError
import one.legalthings.transaction.{Transaction, ValidationError}

object BlockDiffer extends ScorexLogging with Instrumented {

  def fromBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                maybePrevBlock: Option[Block],
                                                block: Block,
                                                constraint: Constraint): Either[ValidationError, (Diff, Constraint)] = {
    val blockSigner = block.signerData.generator.toAddress
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ng4060switchHeight = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)

    lazy val prevBlockFeeDistr: Option[Diff] =
      if (stateHeight > ng4060switchHeight)
        maybePrevBlock.map(prevBlock => Diff.empty.copy(portfolios = Map(blockSigner -> prevBlock.prevBlockFeePart())))
      else None

    lazy val currentBlockFeeDistr =
      if (stateHeight < ng4060switchHeight)
        Some(Diff.empty.copy(portfolios = Map(blockSigner -> block.feesPortfolio())))
      else
        None

    val prevBlockTimestamp = maybePrevBlock.map(_.timestamp)
    for {
      _ <- block.signaturesValid()
      r <- apply(
        settings,
        blockchain,
        constraint,
        prevBlockTimestamp,
        block.signerData.generator,
        prevBlockFeeDistr,
        currentBlockFeeDistr,
        block.timestamp,
        block.transactionData,
        1
      )
    } yield r
  }

  def fromMicroBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                     blockchain: Blockchain,
                                                     prevBlockTimestamp: Option[Long],
                                                     micro: MicroBlock,
                                                     timestamp: Long,
                                                     constraint: Constraint): Either[ValidationError, (Diff, Constraint)] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- Either.cond(blockchain.activatedFeatures.contains(BlockchainFeatures.NG.id), (), ActivationError(s"MicroBlocks are not yet activated"))
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
        0
      )
    } yield r
  }

  private def apply[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                    blockchain: Blockchain,
                                                    initConstraint: Constraint,
                                                    prevBlockTimestamp: Option[Long],
                                                    blockGenerator: Address,
                                                    prevBlockFeeDistr: Option[Diff],
                                                    currentBlockFeeDistr: Option[Diff],
                                                    timestamp: Long,
                                                    txs: Seq[Transaction],
                                                    heightDiff: Int): Either[ValidationError, (Diff, Constraint)] = {
    def updateConstraint(constraint: Constraint, blockchain: Blockchain, tx: Transaction): Constraint =
      constraint.put(blockchain, tx).asInstanceOf[Constraint]

    val currentBlockHeight = blockchain.height + heightDiff
    val txDiffer           = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = currentBlockFeeDistr match {
      case Some(feedistr) =>
        val initDiff = Monoid.combine(prevBlockFeeDistr.orEmpty, feedistr)
        txs.foldLeft((initDiff, initConstraint).asRight[ValidationError]) {
          case (r @ Left(_), _) => r
          case (Right((currDiff, currConstraint)), tx) =>
            val updatedBlockchain = composite(blockchain, currDiff)
            val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
            if (updatedConstraint.isOverfilled) Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
            else
              txDiffer(updatedBlockchain, tx).map { newDiff =>
                (currDiff.combine(newDiff), updatedConstraint)
              }
        }
      case None =>
        txs.foldLeft((prevBlockFeeDistr.orEmpty, initConstraint).asRight[ValidationError]) {
          case (r @ Left(_), _) => r
          case (Right((currDiff, currConstraint)), tx) =>
            val updatedBlockchain = composite(blockchain, currDiff)
            val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
            if (updatedConstraint.isOverfilled) Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
            else
              txDiffer(updatedBlockchain, tx).map { newDiff =>
                val updatedPortfolios = newDiff.portfolios.combine(
                  Map(blockGenerator -> tx.feeDiff()).mapValues(_.multiply(Block.CurrentBlockFeePart))
                )

                (currDiff.combine(newDiff.copy(portfolios = updatedPortfolios)), updatedConstraint)
              }
        }
    }

    txsDiffEi.map {
      case (d, constraint) =>
        val diffWithCancelledLeases =
          if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
            Monoid.combine(d, CancelAllLeases(composite(blockchain, d)))
          else d

        val diffWithLeasePatches =
          if (currentBlockHeight == settings.blockVersion3AfterHeight)
            Monoid.combine(diffWithCancelledLeases, CancelLeaseOverflow(composite(blockchain, diffWithCancelledLeases)))
          else diffWithCancelledLeases

        val diffWithCancelledLeaseIns =
          if (blockchain.featureActivationHeight(BlockchainFeatures.DataTransaction.id).contains(currentBlockHeight))
            Monoid.combine(diffWithLeasePatches, CancelInvalidLeaseIn(composite(blockchain, diffWithLeasePatches)))
          else diffWithLeasePatches

        (diffWithCancelledLeaseIns, constraint)
    }
  }
}
