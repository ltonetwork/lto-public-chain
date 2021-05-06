package com.ltonetwork.state.diffs

import com.ltonetwork.account.{Address, PrivateKeyAccount}
import com.ltonetwork.block.Block
import com.ltonetwork.lagonaki.mocks.TestBlock
import com.ltonetwork.lagonaki.mocks.TestBlock.{create => block}
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.transaction.transfer.TransferTransactionV2
import com.ltonetwork.transaction.{GenesisTransaction, SponsorshipCancelTransaction, SponsorshipTransaction}
import com.ltonetwork.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class SponsorTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  val setup = for {
    sponsor <- accountGen
    sender  <- accountGen
    other   <- accountGen
    ts      <- timestampGen
    sposorTxFee = 5 * 100000000L
    transferTxFee <- enoughFeeGen
    transferAmt   <- positiveLongGen
    g1 = GenesisTransaction.create(sponsor, ENOUGH_AMT, ts).explicitGet()
    g2 = GenesisTransaction.create(sender, ENOUGH_AMT, ts).explicitGet()

    version <- Gen.oneOf(SponsorshipTransaction.supportedVersions.toSeq)
    sponsorship = SponsorshipTransaction.selfSigned(version, sponsor, sender, sposorTxFee, ts + 1).explicitGet()
    cancel      = SponsorshipCancelTransaction.selfSigned(version, sponsor, sender, sposorTxFee, ts + 1).explicitGet()
    transfer    = TransferTransactionV2.selfSigned(2, sender, other, transferAmt, ts + 1, transferTxFee, Array.emptyByteArray).explicitGet()
  } yield (List(g1, g2), sponsorship, cancel, transfer)

  property("sunny day") {
    forAll(setup) {
      case (genesis, sponsorship, cancel, transfer) =>
        assertDiffAndState(Seq(block(genesis), block(Seq(sponsorship))), block(Seq(transfer))) {
          case (d, b) =>
            d.portfolios(sponsorship.sender.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(transfer.recipient.asInstanceOf[Address]).balance shouldBe (transfer.amount)
            val fees = Block.CurrentBlockFeePart(transfer.fee) + sponsorship.fee - Block.CurrentBlockFeePart(sponsorship.fee)
            d.portfolios(TestBlock.defaultSigner).balance shouldBe fees
            b.sponsorOf(transfer.sender.toAddress) shouldBe List(sponsorship.sender.toAddress)
        }
    }
  }
  property("cancel and transfer in one block") {
    forAll(setup) {
      case (genesis, sponsorship, cancel, transfer) =>
        assertDiffAndState(Seq(block(genesis), block(Seq(sponsorship))), block(Seq(cancel, transfer))) {
          case (d, b) =>
            d.portfolios(sponsorship.sender.toAddress).balance shouldBe (-cancel.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.fee - transfer.amount)
            d.portfolios(transfer.recipient.asInstanceOf[Address]).balance shouldBe (transfer.amount)
            b.sponsorOf(transfer.sender.toAddress) shouldBe List.empty
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
            b.sponsorOf(transfer.sender.toAddress) shouldBe List.empty
        }
    }
  }
  property("2 sponsors go in LIFO") {
    val setup2 = for {
      sponsor  <- accountGen
      sponsor2 <- accountGen
      sender   <- accountGen
      other    <- accountGen
      ts       <- positiveLongGen
      sposorTxFee = 5 * 100000000L
      transferTxFee <- enoughFeeGen
      transferAmt   <- positiveLongGen
      g1 = GenesisTransaction.create(sender, ENOUGH_AMT, ts).explicitGet()
      g2 = GenesisTransaction.create(sponsor, ENOUGH_AMT, ts).explicitGet()
      g3 = GenesisTransaction.create(sponsor2, ENOUGH_AMT, ts).explicitGet()

      version <- Gen.oneOf(SponsorshipTransaction.supportedVersions.toSeq)
      sponsorship  = SponsorshipTransaction.selfSigned(version, sponsor, sender, sposorTxFee, ts + 1).explicitGet()
      sponsorship2 = SponsorshipTransaction.selfSigned(version, sponsor2, sender, sposorTxFee, ts + 1).explicitGet()
      cancel       = SponsorshipCancelTransaction.selfSigned(version, sponsor, sender, sposorTxFee, ts + 1).explicitGet()
      transfer     = TransferTransactionV2.selfSigned(2, sender, other, transferAmt, ts + 1, transferTxFee, Array.emptyByteArray).explicitGet()
    } yield (List(g1, g2, g3), sponsorship, sponsorship2, cancel, transfer)

    forAll(setup2) {
      case (genesis, sponsorship, sponsorship2, _, transfer) =>
        assertDiffAndState(Seq(block(genesis), block(Seq(sponsorship)), block(Seq(sponsorship2))), block(Seq(transfer))) {
          case (d, b) =>
            d.portfolios.get(sponsorship.sender.toAddress) shouldBe None
            d.portfolios(sponsorship2.sender.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(transfer.recipient.asInstanceOf[Address]).balance shouldBe (transfer.amount)
            b.sponsorOf(transfer.sender.toAddress) shouldBe List(sponsorship2, sponsorship).map(_.sender.toAddress)
        }
    }
  }
}
