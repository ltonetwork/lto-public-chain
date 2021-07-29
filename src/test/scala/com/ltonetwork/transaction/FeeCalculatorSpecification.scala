package com.ltonetwork.transaction

import com.typesafe.config.ConfigFactory
import com.ltonetwork.TransactionGen
import com.ltonetwork.account.Address
import com.ltonetwork.settings.FeesSettings
import com.ltonetwork.state.{ByteStr, _}
import com.ltonetwork.transaction.association.AssociationTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.sponsorship.SponsorshipTransactionBase
import com.ltonetwork.transaction.transfer._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}

class FeeCalculatorSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen with MockFactory {

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
      |      BASE = 600000000
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
    forAll(transferGen) { tx: TransferTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000)
    }
  }

  property("Mass Transfer transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000 + (tx.transfers.size * 10000))
    }
  }

  property("Lease transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(leaseGen) { tx: LeaseTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 400000)
    }
  }

  property("Association transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(assocTransactionGen) { tx: AssociationTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000000)
    }
  }

  property("Sposnorship transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(Gen.oneOf(sponsorshipGen, cancelSponsorshipGen)) { tx: SponsorshipTransactionBase =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 600000000)
    }
  }

  property("Lease cancel transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(cancelLeaseGen) { tx: CancelLeaseTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 500000)
    }
  }

  property("Data transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(dataTransactionGen) { tx =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= Math.ceil(tx.bytes().length / 1024.0) * 100000)
    }
  }

  private def createBlockchain(accountScript: Address => Option[Script]): Blockchain = {
    val r = stub[Blockchain]
    (r.accountScript _).when(*).onCall((addr: Address) => accountScript(addr)).anyNumberOfTimes()
    r
  }

  private def noScriptBlockchain: Blockchain = createBlockchain(_ => None)
}
