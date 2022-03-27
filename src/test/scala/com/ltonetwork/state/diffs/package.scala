package com.ltonetwork.state

import com.ltonetwork.db.WithState
import com.ltonetwork.mining.MiningConstraint
import com.ltonetwork.settings.FunctionalitySettings
import org.scalatest.matchers.should.Matchers
import com.ltonetwork.block.{Block, TestBlock}
import com.ltonetwork.transaction.{Transaction, ValidationError}
import com.ltonetwork.settings.{TestFunctionalitySettings => TFS}
import com.ltonetwork.state.reader.CompositeBlockchain

package object diffs extends WithState with Matchers {
  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def assertDiffEi(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: Either[ValidationError, Diff] => Unit): Unit = withStateAndHistory(fs) { state =>
    def differ(blockchain: Blockchain, b: Block) = BlockDiffer.fromBlock(fs, blockchain, None, b, MiningConstraint.Unlimited)

    preconditions.foreach { precondition =>
      val (preconditionDiff, preconditionFees, _) = differ(state, precondition).explicitGet()
      state.append(preconditionDiff, preconditionFees, precondition)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1.map(_._1))
  }

  private def assertDiffAndState(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings, withNg: Boolean)(
      assertion: (Diff, Blockchain) => Unit): Unit = withStateAndHistory(fs) { state =>
    def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block) =
      BlockDiffer.fromBlock(fs, blockchain, prevBlock, b, MiningConstraint.Unlimited)

    preconditions.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
      val (diff, fees, _) = differ(state, prevBlock, curBlock).explicitGet()
      state.append(diff, fees, curBlock)
      Some(curBlock)
    }

    val (diff, fees, _) = differ(state, preconditions.lastOption, block).explicitGet()
    val cb              = new CompositeBlockchain(state, Some(diff), 0)
    withClue("[step1 -- asserting composite blockchain] ")(assertion(diff, cb))

    state.append(diff, fees, block)

    withClue("[step2 -- asserting persisted blockchain] ")(assertion(diff, state))
  }

  def assertDiffAndState(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: (Diff, Blockchain) => Unit): Unit =
    assertDiffAndState(preconditions, block, fs, withNg = false)(assertion)

  def assertDiffAndState(fs: FunctionalitySettings)(test: (Seq[Transaction] => Either[ValidationError, Unit]) => Unit): Unit =
    withStateAndHistory(fs) { state =>
      def differ(blockchain: Blockchain, b: Block) = BlockDiffer.fromBlock(fs, blockchain, None, b, MiningConstraint.Unlimited)

      test(txs => {
        val block = TestBlock.create(txs)
        differ(state, block).map(diff => state.append(diff._1, diff._2, block))
      })
    }

  def assertLeft(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(errorMessage: String): Unit =
    assertDiffEi(preconditions, block, fs)(_ should produce(errorMessage))

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  def zipWithPrev[A](seq: Seq[A]): Seq[(Option[A], A)] = {
    seq.zipWithIndex.map { case (a, i) => (if (i == 0) None else Some(seq(i - 1)), a) }
  }
}
