package com.ltonetwork.state.reader

import com.ltonetwork.consensus.GeneratingBalanceProvider
import com.ltonetwork.features.BlockchainFeatures.SmartAccounts
import com.ltonetwork.state.{EitherExt2, LeaseBalance}
import com.ltonetwork.state.diffs._
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.ltonetwork.lagonaki.mocks.TestBlock
import com.ltonetwork.settings.TestFunctionalitySettings.Enabled
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.LeaseTransactionV2

class StateReaderEffectiveBalancePropertyTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
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
      lease1 = LeaseTransactionV2.signed(2, leaser, xfer1.amount - Fee, Fee, ts + 2, master.toAddress, leaser).explicitGet()
      xfer2 <- transferGeneratorPV2(ts + 3, master, leaser.toAddress, ENOUGH_AMT / 3)
      lease2 = LeaseTransactionV2.signed(2, leaser, xfer2.amount - Fee, Fee, ts + 4, master.toAddress, leaser).explicitGet()
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
