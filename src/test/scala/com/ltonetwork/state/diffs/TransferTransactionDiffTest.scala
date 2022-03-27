package com.ltonetwork.state.diffs

import com.ltonetwork.account.Address
import com.ltonetwork.block.TestBlock
import com.ltonetwork.settings.TestFunctionalitySettings
import com.ltonetwork.state.{EitherExt2, LeaseBalance, Portfolio}
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class TransferTransactionDiffTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink {

  private val burnAddress = "3N3pCgpW1cB1Ns56yjPFmXfBSUjNZ1cYroE"
  private val settings = TestFunctionalitySettings.Enabled.copy(
    burnAddresses = Set(burnAddress)
  )

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
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer)), settings) {
          case (totalDiff, newState) =>
            if (transfer.sender.toAddress != transfer.recipient) {
              val senderPortfolio = newState.portfolio(transfer.sender)
              val recipientPortfolio = newState.portfolio(transfer.recipient)

              senderPortfolio shouldBe Portfolio(ENOUGH_AMT - transfer.amount - transfer.fee, LeaseBalance.empty)
              recipientPortfolio shouldBe Portfolio(transfer.amount, LeaseBalance.empty)
            } else {
              newState.portfolio(transfer.sender) shouldBe Portfolio(ENOUGH_AMT - transfer.fee, LeaseBalance.empty)
            }
        }
    }
  }

  val preconditionsAndBurnTransfer: Gen[(GenesisTransaction, TransferTransaction)] = for {
    master    <- accountGen
    recepient = Address.fromString(burnAddress).explicitGet()
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    transfer <- versionedTransferGeneratorP(master, recepient)
  } yield (genesis, transfer)

  property("transfer to burn address") {
    forAll(preconditionsAndBurnTransfer) {
      case (genesis, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer)), settings) {
          case (totalDiff, newState) =>
            val senderPortfolio = newState.portfolio(transfer.sender)

            senderPortfolio shouldBe Portfolio(ENOUGH_AMT - transfer.amount - transfer.fee, LeaseBalance.empty)
            newState.burned shouldBe transfer.amount
        }
    }
  }
}
