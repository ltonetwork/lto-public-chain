package com.ltonetwork.state.diffs

import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.block.{Block, TestBlock}
import TestBlock.{create => block}
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
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class SponsoredTransactionDiffTest
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
    sponsor <- accountGen
    sender  <- accountGen
    other   <- accountGen
    ts      <- timestampGen
    sponsorTxFee = 5 * 100000000L
    transferTxFee <- enoughFeeGen
    transferAmt   <- positiveLongGen
    g1 = GenesisTransaction.create(sponsor, ENOUGH_AMT, ts).explicitGet()
    g2 = GenesisTransaction.create(sender, ENOUGH_AMT, ts).explicitGet()

    version <- Gen.oneOf(SponsorshipTransaction.supportedVersions.toSeq)
    sponsorship = SponsorshipTransaction.signed(version, ts + 1, sponsor, sponsorTxFee, sender).explicitGet()
    cancel      = CancelSponsorshipTransaction.signed(version, ts + 1, sponsor, sponsorTxFee, sender).explicitGet()
    transfer    = TransferTransaction.signed(2, ts + 1, sender, transferTxFee, other, transferAmt, Array.emptyByteArray).explicitGet()
  } yield (List(g1, g2), sponsorship, cancel, transfer, sponsor, sender)

  val setup2 = for {
    sponsor  <- accountGen
    sponsor2 <- accountGen
    sender   <- accountGen
    other    <- accountGen
    ts       <- positiveLongGen
    sponsorTxFee = 5 * 100000000L
    transferTxFee <- enoughFeeGen
    transferAmt   <- positiveLongGen
    g1 = GenesisTransaction.create(sender, ENOUGH_AMT, ts).explicitGet()
    g2 = GenesisTransaction.create(sponsor, ENOUGH_AMT, ts).explicitGet()
    g3 = GenesisTransaction.create(sponsor2, ENOUGH_AMT, ts).explicitGet()

    version <- Gen.oneOf(SponsorshipTransaction.supportedVersions.toSeq)
    sponsorship  = SponsorshipTransaction.signed(version, ts + 1, sponsor, sponsorTxFee, sender).explicitGet()
    sponsorship2 = SponsorshipTransaction.signed(version, ts + 1, sponsor2, sponsorTxFee, sender).explicitGet()
    cancel       = CancelSponsorshipTransaction.signed(version, ts + 1, sponsor, sponsorTxFee, sender).explicitGet()
    transfer     = TransferTransaction.signed(2, ts + 1, sender, transferTxFee, other, transferAmt, Array.emptyByteArray).explicitGet()
  } yield (List(g1, g2, g3), sponsorship, sponsorship2, cancel, transfer, sponsor2)

  property("sponsored account") {
    forAll(setup) {
      case (genesis, sponsorship, _, transfer, _, _) =>
        assertDiffAndState(Seq(block(genesis), block(sponsorship)), block(transfer)) {
          case (d, b) =>
            d.portfolios(sponsorship.sender.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(transfer.recipient).balance shouldBe transfer.amount
            val fees = Block.CurrentBlockFeePart(transfer.fee) + sponsorship.fee - Block.CurrentBlockFeePart(sponsorship.fee)
            d.portfolios(TestBlock.defaultSigner).balance shouldBe fees
            b.sponsorOf(transfer.sender.toAddress) shouldBe List(sponsorship.sender.toAddress)
        }
    }
  }

  property("cancel sponsorship and transfer in one block") {
    forAll(setup) {
      case (genesis, sponsorship, cancel, transfer, _, _) =>
        assertDiffAndState(Seq(block(genesis), block(sponsorship)), block(cancel, transfer)) {
          case (d, b) =>
            d.portfolios(sponsorship.sender.toAddress).balance shouldBe (-cancel.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.fee - transfer.amount)
            d.portfolios(transfer.recipient).balance shouldBe transfer.amount
            b.sponsorOf(transfer.sender.toAddress) shouldBe List.empty
        }
    }
  }

  property("cancel sponsorship and transfer in different blocks") {
    forAll(setup) {
      case (genesis, sponsorship, cancel, transfer, _, _) =>
        assertDiffAndState(Seq(block(genesis), block(sponsorship), block(cancel)), block(transfer)) {
          case (d, b) =>
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.fee - transfer.amount)
            d.portfolios(transfer.recipient).balance shouldBe transfer.amount
            b.sponsorOf(transfer.sender.toAddress) shouldBe List.empty
        }
    }
  }

  property("2 sponsors go in LIFO") {
    forAll(setup2) {
      case (genesis, sponsorship, sponsorship2, _, transfer, _) =>
        assertDiffAndState(Seq(block(genesis), block(sponsorship), block(sponsorship2)), block(transfer)) {
          case (d, b) =>
            d.portfolios.get(sponsorship.sender.toAddress) shouldBe None
            d.portfolios(sponsorship2.sender.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(transfer.recipient).balance shouldBe transfer.amount
            b.sponsorOf(transfer.sender.toAddress) shouldBe List(sponsorship2, sponsorship).map(_.sender.toAddress)
        }
    }
  }

  property("2 sponsors go in LIFO with fallthrough") {
    forAll(setup2) {
      case (genesis, sponsorship, sponsorship2, _, transfer, sponsor2) =>
        val transferSponsor2 = TransferTransaction
          .signed(
            version = 2,
            sponsorship2.timestamp,
            sponsor2,
            transfer.fee,
            transfer.recipient,
            ENOUGH_AMT - sponsorship2.fee - transfer.fee,
            Array.emptyByteArray
          )
          .explicitGet()

        assertDiffAndState(Seq(block(genesis), block(sponsorship), block(sponsorship2), block(transferSponsor2)), block(transfer)) {
          case (d, b) =>
            d.portfolios.get(sponsorship2.sender.toAddress) shouldBe None
            d.portfolios(sponsorship.sender.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(transfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(transfer.recipient).balance shouldBe transfer.amount
            b.sponsorOf(transfer.sender.toAddress) shouldBe List(sponsorship2, sponsorship).map(_.sender.toAddress)
        }
    }
  }

  property("sponsored transaction") {
    forAll(setup) {
      case (genesis, _, _, transfer, sponsor, _) =>
        val sponsoredTransfer = transfer.sponsorWith(sponsor)

        assertDiffAndState(Seq(block(genesis)), block(sponsoredTransfer)) {
          case (d, b) =>
            d.portfolios(sponsor.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(sponsoredTransfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(sponsoredTransfer.recipient).balance shouldBe transfer.amount
            val fees = Block.CurrentBlockFeePart(sponsoredTransfer.fee)
            d.portfolios(TestBlock.defaultSigner).balance shouldBe fees
        }
    }
  }

  property("sponsored transaction with sponsored account") {
    forAll(setup2) {
      case (genesis, sponsorship, _, _, transfer, sponsor) =>
        val sponsoredTransfer = transfer.sponsorWith(sponsor)

        assertDiffAndState(Seq(block(genesis), block(sponsorship)), block(sponsoredTransfer)) {
          case (d, b) =>
            d.portfolios(sponsor.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(sponsoredTransfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(sponsoredTransfer.recipient).balance shouldBe transfer.amount
            val fees = Block.CurrentBlockFeePart(transfer.fee) + sponsorship.fee - Block.CurrentBlockFeePart(sponsorship.fee)
            d.portfolios(TestBlock.defaultSigner).balance shouldBe fees
            b.sponsorOf(transfer.sender.toAddress) shouldBe List(sponsorship.sender.toAddress)
        }
    }
  }

  property("sponsored transaction for smart account that accepts tx") {
    forAll(setup) {
      case (genesis, _, _, transfer, sponsor, sender) =>
        val script    = ScriptV1(TRUE).explicitGet()
        val setScript = SetScriptTransaction.signed(1, transfer.timestamp, sender, 10.lto, Some(script)).explicitGet()

        val sponsoredTransfer = transfer.sponsorWith(sponsor)

        assertDiffAndState(Seq(block(genesis), block(setScript)), block(sponsoredTransfer), smartEnabledFS) {
          case (d, _) =>
            d.portfolios(sponsor.toAddress).balance shouldBe (-transfer.fee)
            d.portfolios(sponsoredTransfer.sender.toAddress).balance shouldBe (-transfer.amount)
            d.portfolios(sponsoredTransfer.recipient).balance shouldBe transfer.amount
            val fees = Block.CurrentBlockFeePart(transfer.fee) + setScript.fee - Block.CurrentBlockFeePart(setScript.fee)
            d.portfolios(TestBlock.defaultSigner).balance shouldBe fees
        }
    }
  }

  property("sponsored transaction for smart account that rejects tx") {
    forAll(setup) {
      case (genesis, _, _, transfer, sponsor, sender) =>
        val script    = ScriptV1(FALSE).explicitGet()
        val setScript = SetScriptTransaction.signed(1, transfer.timestamp, sender, 10.lto, Some(script)).explicitGet()

        val sponsoredTransfer = transfer.sponsorWith(sponsor)

        assertDiffEi(Seq(block(genesis), block(setScript)), block(sponsoredTransfer), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

  property("smart account that sponsors a transaction") {
    forAll(setup) {
      case (genesis, _, _, transfer, sponsor, _) =>
        val script    = ScriptV1(TRUE).explicitGet()
        val setScript = SetScriptTransaction.signed(1, transfer.timestamp, sponsor, 10.lto, Some(script)).explicitGet()

        val sponsoredTransfer = transfer.sponsorWith(sponsor)

        assertDiffEi(Seq(block(genesis), block(setScript)), block(sponsoredTransfer), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("Transactions can't be sponsored by a scripted account"))
    }
  }
}
