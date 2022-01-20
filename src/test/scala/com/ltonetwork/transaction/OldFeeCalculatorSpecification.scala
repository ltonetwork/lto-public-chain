package com.ltonetwork.transaction

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.Address
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.settings.FeesSettings
import com.ltonetwork.state._
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer._
import com.typesafe.config.ConfigFactory
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

// Pre-tokenomics-redefined, using the configured fees.

class OldFeeCalculatorSpecification extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with MockFactory {

  private val configString =
    """lto {
      |  fees {
      |    transfer {
      |      BASE = 100000
      |    }
      |    mass-transfer {
      |      BASE = 100000
      |      VAR  = 10000
      |    }
      |    lease {
      |      BASE = 400000
      |    }
      |    cancel-lease {
      |      BASE = 500000
      |    }
      |    data {
      |      BASE = 100000
      |      VAR = 10000
      |    }
      |    issue-association {
      |      BASE = 100000000
      |    }
      |    revoke-association {
      |      BASE = 100000000
      |    }
      |
      |    sponsorship {
      |      BASE = 600000000
      |    }
      |    cancel-sponsorship {
      |      BASE = 100000000
      |    }
      |  }
      |}""".stripMargin

  private val config = ConfigFactory.parseString(configString)

  private val mySettings = FeesSettings.fromConfig(config)

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
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(transferGen, Gen.choose[Long](80000, 120000)) { (tx: TransferTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 100000)
    }
  }

  property("Mass Transfer transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(massTransferGen(10), Gen.choose[Long](80000, 160000)) { (tx: MassTransferTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 100000 + (tx.transfers.size * 10000))
    }
  }

  property("Lease transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(leaseGen, Gen.choose[Long](300000, 500000)) { (tx: LeaseTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 400000)
    }
  }

  property("Association transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(assocTransactionGen, Gen.choose[Long](80000000, 120000000)) { (tx: AssociationTransaction, fee: Long) =>
      val txWithFee = tx match {
        case iatx: IssueAssociationTransaction => iatx.copy(fee = fee)
        case ratx: RevokeAssociationTransaction => ratx.copy(fee = fee)
      }
      feeCalc.enoughFee(txWithFee) shouldBeRightIf (fee >= 100000000)
    }
  }

  property("Sponsorship transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(sponsorshipGen, Gen.choose[Long](400000000, 800000000)) { (tx: SponsorshipTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 600000000)
    }
  }

  property("Cancel Sponsorship transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(cancelSponsorshipGen, Gen.choose[Long](80000000, 120000000)) { (tx: CancelSponsorshipTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 100000000)
    }
  }

  property("Lease cancel transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(cancelLeaseGen, Gen.choose[Long](500000, 600000)) { (tx: CancelLeaseTransaction, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 500000)
    }
  }

  property("Data transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(dataTransactionGen(4), Gen.choose[Long](80000, 200000)) { (tx, fee: Long) =>
      feeCalc.enoughFee(tx.copy(fee = fee)) shouldBeRightIf (fee >= 100000 + Math.ceil(tx.bytes().length / 1024.0) * 10000)
    }
  }

  private def createBlockchain(accountScript: Address => Option[Script]): Blockchain = {
    val r = stub[Blockchain]
    (r.accountScript _).when(*).onCall((addr: Address) => accountScript(addr)).anyNumberOfTimes()
    (r.activatedFeatures _).when().returns(Map(BlockchainFeatures.Cobalt.id -> 0))
    r
  }

  private def noScriptBlockchain: Blockchain = createBlockchain(_ => None)
}
