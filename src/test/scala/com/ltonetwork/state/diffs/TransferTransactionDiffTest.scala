package com.ltonetwork.state.diffs

import com.ltonetwork.account.Address
import com.ltonetwork.block.TestBlock
import com.ltonetwork.state.{EitherExt2, LeaseBalance, Portfolio}
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class TransferTransactionDiffTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndTransfer: Gen[(GenesisTransaction, TransferTransaction)] = for {
    master    <- accountGen
    recepient <- otherAccountGen(candidate = master)
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    transferV1 <- transferGeneratorP(master, recepient)
    transferV2 <- versionedTransferGeneratorP(master, recepient)
    transfer   <- Gen.oneOf(transferV1, transferV2)
  } yield (genesis, transfer)

  property("transfers assets to recipient preserving lto invariant") {
    forAll(preconditionsAndTransfer) {
      case (genesis, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer))) {
          case (totalDiff, newState) =>
            val recipient: Address = transfer.recipient
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
      transferV1 <- transferGeneratorP(master, recepient)
      transferV2 <- transferGeneratorP(master, recepient)
      transfer   <- Gen.oneOf(transferV1, transferV2)
    } yield (genesis, transfer)
  }
}
