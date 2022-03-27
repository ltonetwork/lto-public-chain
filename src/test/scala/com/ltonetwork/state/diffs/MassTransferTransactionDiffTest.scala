package com.ltonetwork.state.diffs

import com.ltonetwork.state.{EitherExt2, LeaseBalance, Portfolio}
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import com.ltonetwork.account.{Address, PrivateKeyAccount}
import com.ltonetwork.settings.TestFunctionalitySettings
import com.ltonetwork.block.TestBlock.{create => block}
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.transfer.MassTransferTransaction.ParsedTransfer

class MassTransferTransactionDiffTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink {

  val fs = TestFunctionalitySettings.Enabled

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master)

  property("MassTransfer preserves balance invariant") {
    def testDiff(transferCount: Int): Unit = {
      val setup = for {
        (genesis, master) <- baseSetup
        transferGen = for {
          recipient <- accountGen.map(_.toAddress)
          amount    <- Gen.choose(100000L, 1000000000L)
        } yield ParsedTransfer(recipient, amount)
        transfers <- Gen.listOfN(transferCount, transferGen)
        transfer  <- massTransferGeneratorP(master, transfers)
      } yield (genesis, transfer)

      forAll(setup) {
        case (genesis, transfer) =>
          assertDiffAndState(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) {
            case (totalDiff, newState) =>
              val totalAmount     = transfer.transfers.map(_.amount).sum
              val fees            = transfer.fee
              val senderPortfolio = newState.portfolio(transfer.sender)
              senderPortfolio.balance shouldBe (ENOUGH_AMT - fees - totalAmount)
              for (ParsedTransfer(recipient, amount) <- transfer.transfers) {
                val recipientPortfolio = newState.portfolio(recipient.asInstanceOf[Address])
                if (transfer.sender.toAddress != recipient) {
                  recipientPortfolio shouldBe Portfolio(amount, LeaseBalance.empty)
                }
              }
          }
      }
    }

    import com.ltonetwork.transaction.transfer.MassTransferTransaction.{MaxTransferCount => Max}
    Seq(0, 1, Max) foreach testDiff // test edge cases
    Gen.choose(2, Max - 1) map testDiff
  }

  property("MassTransfer cannot overspend funds") {
    val setup = for {
      (genesis, master) <- baseSetup
      recipients        <- Gen.listOfN(2, accountGen.map(acc => ParsedTransfer(acc.toAddress, ENOUGH_AMT / 2 + 1)))
      transfer          <- massTransferGeneratorP(master, recipients)
    } yield (genesis, transfer)

    forAll(setup) {
      case (genesis, transfer) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
          blockDiffEi should produce("Attempt to transfer unavailable funds")
        }
    }
  }

}
