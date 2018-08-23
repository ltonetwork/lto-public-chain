package one.legalthings.state.diffs

import cats.implicits._
import one.legalthings.state.{EitherExt2, LeaseBalance, Portfolio}
import one.legalthings.TransactionGen
import one.legalthings.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import one.legalthings.account.Address
import one.legalthings.lagonaki.mocks.TestBlock
import one.legalthings.transaction.GenesisTransaction
import one.legalthings.transaction.ValidationError.GenericError
import one.legalthings.transaction.assets._
import one.legalthings.transaction.transfer._

class TransferTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndTransfer: Gen[(GenesisTransaction, TransferTransaction)] = for {
    master    <- accountGen
    recepient <- otherAccountGen(candidate = master)
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, master).map(_._1)
    transferV1               <- transferGeneratorP(master, recepient, None, None)
    transferV2               <- versionedTransferGeneratorP(master, recepient, None, None)
    transfer                 <- Gen.oneOf(transferV1, transferV2)
  } yield (genesis, transfer)

  property("transfers assets to recipient preserving waves invariant") {
    forAll(preconditionsAndTransfer) {
      case ((genesis, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer))) {
          case (totalDiff, newState) =>
            assertBalanceInvariant(totalDiff)

            val recipient: Address = transfer.recipient.asInstanceOf[Address]
            val recipientPortfolio = newState.portfolio(recipient)
            if (transfer.sender.toAddress != recipient) {
              recipientPortfolio shouldBe Portfolio(transfer.amount, LeaseBalance.empty)
            }
        }
    }
  }

  val transferWithSmartAssetFee: Gen[(GenesisTransaction, TransferTransaction)] = {
    for {
      master    <- accountGen
      recepient <- otherAccountGen(master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      transferV1 <- transferGeneratorP(master, recepient, None, None)
      transferV2 <- transferGeneratorP(master, recepient, None, None)
      transfer   <- Gen.oneOf(transferV1, transferV2)
    } yield (genesis, transfer)
  }
}
