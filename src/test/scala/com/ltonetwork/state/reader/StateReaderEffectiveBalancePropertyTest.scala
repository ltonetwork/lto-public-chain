package com.ltonetwork.state.reader

import com.ltonetwork.block.TestBlock
import com.ltonetwork.consensus.GeneratingBalanceProvider
import com.ltonetwork.features.BlockchainFeatures.SmartAccounts
import com.ltonetwork.state.{EitherExt2, LeaseBalance}
import com.ltonetwork.state.diffs._
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import com.ltonetwork.settings.TestFunctionalitySettings.Enabled
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.LeaseTransaction

class StateReaderEffectiveBalancePropertyTest
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink {
  property("No-interactions genesis account's effectiveBalance doesn't depend on depths") {
    val setup: Gen[(GenesisTransaction, Int, Int, Int)] = for {
      master <- accountGen
      ts     <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      emptyBlocksAmt <- Gen.choose(1, 10)
      atHeight       <- Gen.choose(1, 20)
      confirmations  <- Gen.choose(1, 20)
    } yield (genesis, emptyBlocksAmt, atHeight, confirmations)

    forAll(setup) {
      case (genesis: GenesisTransaction, emptyBlocksAmt, atHeight, confirmations) =>
        val genesisBlock = TestBlock.create(Seq(genesis))
        val nextBlocks   = List.fill(emptyBlocksAmt - 1)(TestBlock.create(Seq.empty))
        assertDiffAndState(genesisBlock +: nextBlocks, TestBlock.create(Seq.empty)) { (_, newState) =>
          newState.effectiveBalance(genesis.recipient, confirmations) shouldBe genesis.amount
        }
    }
  }

  property("Negative generating balance case") {
    val fs  = Enabled.copy(preActivatedFeatures = Map(SmartAccounts.id -> 0))
    val Fee = 100000000
    val setup = for {
      master <- accountGen
      ts     <- positiveLongGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      leaser <- accountGen
      xfer1  <- transferGeneratorPV2(ts + 1, master, leaser.toAddress, ENOUGH_AMT / 3)
      lease1 = LeaseTransaction.signed(2, ts + 2, leaser, Fee, master.toAddress, xfer1.amount - Fee).explicitGet()
      xfer2 <- transferGeneratorPV2(ts + 3, master, leaser.toAddress, ENOUGH_AMT / 3)
      lease2 = LeaseTransaction.signed(2, ts + 4, leaser, Fee, master.toAddress, xfer2.amount - Fee).explicitGet()
    } yield (leaser, genesis, xfer1, lease1, xfer2, lease2)

    forAll(setup) {
      case (leaser, genesis, xfer1, lease1, xfer2, lease2) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(xfer1, lease1))), TestBlock.create(Seq(xfer2, lease2)), fs) {
          (_, state) =>
            val portfolio       = state.portfolio(lease1.sender)
            val expectedBalance = xfer1.amount + xfer2.amount - 2 * Fee
            portfolio.balance shouldBe expectedBalance
            GeneratingBalanceProvider.balance(state, fs, leaser, state.lastBlockId.get) shouldBe 0
            portfolio.lease shouldBe LeaseBalance(0, expectedBalance)
            portfolio.effectiveBalance shouldBe 0
        }
    }
  }
}
