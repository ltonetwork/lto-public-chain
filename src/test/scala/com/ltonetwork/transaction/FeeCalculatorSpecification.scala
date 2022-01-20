package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.Address
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.settings.FeesSettings
import com.ltonetwork.state._
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction, SponsorshipTransactionBase}
import com.ltonetwork.transaction.transfer._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import com.ltonetwork.utils._

class FeeCalculatorSpecification extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with MockFactory {

  implicit class ConditionalAssert(v: Either[_, _]) {

    def shouldBeRightIf(cond: Boolean): Assertion = {
      if (cond) {
        v shouldBe an[Right[_, _]]
      } else {
        v shouldBe an[Left[_, _]]
      }
    }
  }

  property("Transfer transaction") {
    val feeCalc = new FeeCalculator(FeesSettings.empty, noScriptBlockchain)
    forAll(transferGen, Gen.choose(0.008.lto, 0.012.lto)) { (tx: TransferTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 0.01.lto)
    }
  }

  property("Mass Transfer transaction") {
    val feeCalc = new FeeCalculator(FeesSettings.empty, noScriptBlockchain)
    forAll(massTransferGen(4), Gen.choose(0.008.lto, 0.02.lto)) { (tx: MassTransferTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 0.01.lto + (tx.transfers.size * 0.001.lto))
    }
  }

  property("Lease transaction") {
    val feeCalc = new FeeCalculator(FeesSettings.empty, noScriptBlockchain)
    forAll(leaseGen, Gen.choose(0.008.lto, 0.012.lto)) { (tx: LeaseTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 0.01.lto)
    }
  }

  property("Association transaction") {
    val feeCalc = new FeeCalculator(FeesSettings.empty, noScriptBlockchain)
    forAll(assocTransactionGen, Gen.choose(0.008.lto, 0.012.lto)) { (tx: AssociationTransaction, fee: Long) =>
      val txWithFee = tx match {
        case iatx: IssueAssociationTransaction => iatx.copy(fee = fee)
        case ratx: RevokeAssociationTransaction => ratx.copy(fee = fee)
      }
      feeCalc.enoughFee(txWithFee) shouldBeRightIf (fee >= 0.01.lto)
    }
  }

  property("Sponsorship transaction") {
    val feeCalc = new FeeCalculator(FeesSettings.empty, noScriptBlockchain)
    forAll(sponsorshipGen, Gen.choose(0.08.lto, 0.12.lto)) { (tx: SponsorshipTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 0.1.lto)
    }
  }

  property("Cancel Sponsorship transaction") {
    val feeCalc = new FeeCalculator(FeesSettings.empty, noScriptBlockchain)
    forAll(cancelSponsorshipGen, Gen.choose(0.08.lto, 0.12.lto)) { (tx: CancelSponsorshipTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 0.1.lto)
    }
  }

  property("Lease cancel transaction") {
    val feeCalc = new FeeCalculator(FeesSettings.empty, noScriptBlockchain)
    forAll(cancelLeaseGen, Gen.choose(0.008.lto, 0.012.lto)) { (tx: CancelLeaseTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 0.01.lto)
    }
  }

  property("Data transaction") {
    val feeCalc = new FeeCalculator(FeesSettings.empty, noScriptBlockchain)
    forAll(dataTransactionGen(10), Gen.choose(0.008.lto, 0.02.lto)) { (tx, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 0.01.lto + Math.ceil(tx.bytes().length / 1024*256) * 0.001.lto)
    }
  }

  private def createBlockchain(accountScript: Address => Option[Script]): Blockchain = {
    val r = stub[Blockchain]
    (r.accountScript _).when(*).onCall((addr: Address) => accountScript(addr)).anyNumberOfTimes()
    (r.activatedFeatures _).when().returns(Map(
      BlockchainFeatures.TokenomicsRedefined.id -> 0,
      BlockchainFeatures.Cobalt.id -> 0,
    ))
    r
  }

  private def noScriptBlockchain: Blockchain = createBlockchain(_ => None)
}
