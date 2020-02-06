package com.wavesplatform.state.diffs

import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import com.wavesplatform.transaction.{GenesisTransaction, SponsorshipCancelTransaction, SponsorshipTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class SponsorTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  val setup = for {
    sponsor <- accountGen
    sender  <- accountGen
    other   <- accountGen
    ts      <- positiveLongGen
    sposorTxFee = 5 * 100000000L
    transferTxFee <- enoughFeeGen
    transferAmt   <- positiveLongGen
    g1 = GenesisTransaction.create(sponsor, ENOUGH_AMT, ts).explicitGet()
    g2 = GenesisTransaction.create(sender, ENOUGH_AMT, ts).explicitGet()

    version <- Gen.oneOf(SponsorshipTransaction.supportedVersions.toSeq)
    sponsorship = SponsorshipTransaction.selfSigned(version, sponsor, sender, sposorTxFee, ts + 1).explicitGet()
    cancel = SponsorshipCancelTransaction.selfSigned(version, sponsor, sender, sposorTxFee, ts + 1).explicitGet()
    transfer = TransferTransactionV2.selfSigned(2, sender, other, transferAmt, transferTxFee, ts + 1, Array.emptyByteArray).explicitGet()
  } yield (List(g1, g2), sponsorship, cancel, transfer)



  property("sunny day") {
    forAll(setup) {
      case (genesis, sponsorship, cancel, transfer) =>
        assertDiffAndState(Seq(block(genesis), block(Seq(sponsorship))), block(Seq(transfer))) {
          case (d, b) =>
            d.portfolios(sponsorship.sender.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(transfer.recipient.asInstanceOf[Address]).balance shouldBe (transfer.amount)
            d.portfolios(TestBlock.defaultSigner).balance shouldBe (transfer.fee)
            b.sponsorOf(transfer.sender.toAddress) shouldBe Some(sponsorship.sender.toAddress)
        }
    }
  }
  property("cancel and transfer in one block") {
    forAll(setup) {
      case (genesis, sponsorship, cancel, transfer) =>
        assertDiffAndState(Seq(block(genesis), block(Seq(sponsorship))), block(Seq(cancel,transfer))) {
          case (d, b) =>
            d.portfolios(sponsorship.sender.toAddress).balance shouldBe (-cancel.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.fee - transfer.amount)
            d.portfolios(transfer.recipient.asInstanceOf[Address]).balance shouldBe (transfer.amount)
            d.portfolios(TestBlock.defaultSigner).balance shouldBe (cancel.fee + transfer.fee)
            b.sponsorOf(transfer.sender.toAddress) shouldBe None
        }
    }
  }
  property("cancel and transfer in different blocks") {
    forAll(setup) {
      case (genesis, sponsorship, cancel, transfer) =>
        assertDiffAndState(Seq(block(genesis), block(Seq(sponsorship)), block(Seq(cancel))), block(Seq(transfer))) {
          case (d, b) =>
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.fee - transfer.amount)
            d.portfolios(transfer.recipient.asInstanceOf[Address]).balance shouldBe (transfer.amount)
            d.portfolios(TestBlock.defaultSigner).balance shouldBe (transfer.fee)
            b.sponsorOf(transfer.sender.toAddress) shouldBe None
        }
    }
  }
}
