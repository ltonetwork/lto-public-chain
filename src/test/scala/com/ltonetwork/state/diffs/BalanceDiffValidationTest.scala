package com.ltonetwork.state.diffs

import com.ltonetwork.block.TestBlock
import com.ltonetwork.settings.TestFunctionalitySettings
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.transfer._

class BalanceDiffValidationTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink {

  property("disallows overflow") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, GenesisTransaction, TransferTransaction, TransferTransaction)] = for {
      master    <- accountGen
      master2   <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts        <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(master, Long.MaxValue - 1, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(master2, Long.MaxValue - 1, ts).explicitGet()
      fee    <- smallFeeGen
      amount <- Gen.choose(Long.MaxValue / 2, Long.MaxValue - fee - 1)
      transfer1 = createLtoTransfer(master, recipient, amount, fee, ts).explicitGet()
      transfer2 = createLtoTransfer(master2, recipient, amount, fee, ts).explicitGet()
    } yield (gen1, gen2, transfer1, transfer2)

    forAll(preconditionsAndPayment) {
      case (gen1, gen2, transfer1, transfer2) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, transfer1))), TestBlock.create(Seq(transfer2))) { blockDiffEi =>
          blockDiffEi should produce("negative lto balance")
        }
    }
  }

  property("disallows lease overflow") {
    val setup = for {
      master1   <- accountGen
      master2   <- accountGen
      recipient <- accountGen
      ts        <- timestampGen
      gen1 = GenesisTransaction.create(master1, Long.MaxValue - 1, ts).explicitGet()
      gen2 = GenesisTransaction.create(master2, Long.MaxValue - 1, ts).explicitGet()
      fee  <- smallFeeGen
      amt1 <- Gen.choose(Long.MaxValue / 2 + 1, Long.MaxValue - 1 - fee)
      amt2 <- Gen.choose(Long.MaxValue / 2 + 1, Long.MaxValue - 1 - fee)
      l1   <- createLease(master1, amt1, fee, ts, recipient)
      l2   <- createLease(master2, amt2, fee, ts, recipient)
    } yield (gen1, gen2, l1, l2)

    forAll(setup) {
      case (gen1, gen2, l1, l2) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, l1))), TestBlock.create(Seq(l2)))(totalDiffEi =>
          totalDiffEi should produce("negative effective balance"))
    }
  }

  val ownLessThatLeaseOut: Gen[(GenesisTransaction, TransferTransaction, LeaseTransaction, LeaseTransaction, TransferTransaction)] = for {
    master <- accountGen
    alice  <- accountGen
    bob    <- accountGen
    cooper <- accountGen
    ts     <- positiveIntGen
    amt    <- positiveLongGen
    fee    <- smallFeeGen
    genesis: GenesisTransaction                 = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    masterTransfersToAlice: TransferTransaction = createLtoTransfer(master, alice, amt, fee, ts).explicitGet()
    (aliceLeasesToBob, _)    <- leaseAndCancelGeneratorP(alice, bob, alice, ts) suchThat (_._1.amount < amt)
    (masterLeasesToAlice, _) <- leaseAndCancelGeneratorP(master, alice, master, ts) suchThat (_._1.amount > aliceLeasesToBob.amount)
    transferAmt              <- Gen.choose(amt - fee - aliceLeasesToBob.amount, amt - fee)
    aliceTransfersMoreThanOwnsMinusLeaseOut = createLtoTransfer(alice, cooper, transferAmt, fee, ts).explicitGet()

  } yield (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut)

  property("cannot transfer more than own-leaseOut") {
    val settings = TestFunctionalitySettings.Enabled

    forAll(ownLessThatLeaseOut) {
      case (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut) =>
        assertDiffEi(
          Seq(
            TestBlock.create(Seq(genesis)),
            TestBlock.create(Seq()),
            TestBlock.create(Seq()),
            TestBlock.create(Seq()),
            TestBlock.create(Seq(masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice))
          ),
          TestBlock.create(Seq(aliceTransfersMoreThanOwnsMinusLeaseOut)),
          settings
        ) { totalDiffEi =>
          totalDiffEi should produce("trying to spend leased money")
        }
    }
  }
}
