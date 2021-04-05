package com.wavesplatform.state.diffs

import com.wavesplatform.account.Address
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.{EitherExt2, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class TransferTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndTransfer: Gen[(GenesisTransaction, TransferTransaction)] = for {
    master    <- accountGen
    recepient <- otherAccountGen(candidate = master)
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    transferV1 <- transferGeneratorP(master, recepient, None, None)
    transferV2 <- versionedTransferGeneratorP(master, recepient, None, None)
    transfer   <- Gen.oneOf(transferV1, transferV2)
  } yield (genesis, transfer)

  property("transfers assets to recipient preserving waves invariant") {
    forAll(preconditionsAndTransfer) {
      case ((genesis, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer))) {
          case (totalDiff, newState) =>
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
