package one.legalthings.state.patch

import one.legalthings.settings.TestFunctionalitySettings
import one.legalthings.state.EitherExt2
import one.legalthings.state.diffs._
import one.legalthings.TransactionGen
import one.legalthings.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import one.legalthings.lagonaki.mocks.TestBlock
import one.legalthings.transaction.GenesisTransaction
import one.legalthings.transaction.lease.LeaseTransactionV1
import one.legalthings.transaction.transfer._

class CancelLeaseOverflowTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 5)

  property("CancelLeaseOverflow cancels active outgoing leases for accounts having negative spendable balances") {
    val leaseOverflowGen = for {
      sender1   <- accountGen
      sender2   <- accountGen
      recipient <- accountGen
      amount    <- positiveLongGen
      fee       <- smallFeeGen
      ts        <- timestampGen
    } yield
      (
        GenesisTransaction.create(sender1, amount + fee, ts).explicitGet(),
        GenesisTransaction.create(sender2, amount + fee * 2, ts).explicitGet(),
        LeaseTransactionV1.selfSigned(sender1, amount, fee, ts, sender2).explicitGet(),
        LeaseTransactionV1.selfSigned(sender2, amount, fee, ts, recipient).explicitGet(),
        TransferTransactionV1.selfSigned(sender2, recipient, amount, ts, fee, Array.emptyByteArray).explicitGet()
      )

    forAll(leaseOverflowGen) {
      case (gt1, gt2, lease1, lease2, tx) =>
        assertDiffAndState(
          Seq(TestBlock.create(Seq(gt1, gt2)), TestBlock.create(Seq(lease1)), TestBlock.create(Seq(lease2, tx)), TestBlock.create(Seq.empty)),
          TestBlock.create(Seq.empty),
          settings
        ) {
          case (_, newState) =>
            newState.leaseDetails(lease2.id()).forall(_.isActive) shouldBe false
            newState.leaseDetails(lease1.id()).exists(_.isActive) shouldBe true
        }
    }
  }
}
