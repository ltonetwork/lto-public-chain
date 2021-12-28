package com.ltonetwork.state.diffs

import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.block.Block
import com.ltonetwork.lagonaki.mocks.TestBlock
import com.ltonetwork.lagonaki.mocks.TestBlock.{create => block}
import com.ltonetwork.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.state.diffs.smart.smartEnabledFS
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer.TransferTransaction
import com.ltonetwork.utils._
import com.ltonetwork.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SetScriptTransactionDiffTest
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with WithDB {

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  val setup = for {
    sender        <- accountGen
    recipient     <- accountGen
    ts            <- timestampGen
    transferTxFee <- enoughFeeGen
    transferAmt   <- positiveLongGen
    genesis = GenesisTransaction.create(sender, ENOUGH_AMT, ts).explicitGet()

    script   = setScriptTransactionGenTest().pureApply(Parameters.default, Seed.random())
    transfer = TransferTransaction.signed(2, ts + 1, sender, transferTxFee, recipient, transferAmt, Array.emptyByteArray).explicitGet()
  } yield (genesis, script, transfer, sender)

  property("opalaaaa") {
    forAll(setup) {
      case (genesis, script, transfer, sender) =>
        assertDiffAndState(Seq(block(genesis), block(script)), block(transfer)) {
          case (d, b) =>
            d.portfolios(script.sender.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(transfer.recipient).balance shouldBe transfer.amount
            val fees = Block.CurrentBlockFeePart(transfer.fee) + script.fee - Block.CurrentBlockFeePart(script.fee)
            d.portfolios(TestBlock.defaultSigner).balance shouldBe fees
            b.sponsorOf(transfer.sender.toAddress) shouldBe List(script.sender.toAddress)
        }
    }
  }

}
