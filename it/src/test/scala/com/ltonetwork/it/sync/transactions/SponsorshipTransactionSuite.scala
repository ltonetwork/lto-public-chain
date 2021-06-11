package com.ltonetwork.it.sync.transactions

import com.typesafe.config.{Config, ConfigFactory}
import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.account.PublicKeyAccount._
import com.ltonetwork.api.http.SponsorshipApiRoute.SponsorshipInfo
import com.ltonetwork.it.NodeConfigs.Default
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.transactions.BaseTransactionSuite
import com.ltonetwork.it.util._
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.transaction.sponsorship.SponsorshipTransaction
import com.ltonetwork.transaction.transfer.TransferTransaction
import org.scalatest.CancelAfterFailure

class SponsorshipTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  val fee = 1.lto

  val payer     = PrivateKeyAccount.fromSeed("sender").explicitGet()
  val recipient = PrivateKeyAccount.fromSeed("recipient").explicitGet()
  val sponsor   = PrivateKeyAccount.fromSeed("sponsor").explicitGet()

  override protected def nodeConfigs: Seq[Config] = SponsorshipTransactionSuite.Configs

  test("sponsor pays for sender") {

    val topUpPayer =
      TransferTransaction.selfSigned(2, System.currentTimeMillis(), sender.privateKey, fee, payer, 10.lto, Array.emptyByteArray).explicitGet()
    val topUpSponsor =
      TransferTransaction.selfSigned(2, System.currentTimeMillis(), sender.privateKey, fee, sponsor, 10.lto, Array.emptyByteArray).explicitGet()
    val becomeSponsor = SponsorshipTransaction.selfSigned(1, System.currentTimeMillis(), sponsor, 5 * fee, payer).explicitGet()
    val makePayment =
      TransferTransaction.selfSigned(2, System.currentTimeMillis(), payer, fee, recipient, 4.lto, Array.emptyByteArray).explicitGet()

    val topUpPayerId = sender.signedBroadcast(topUpPayer.json()).id
    nodes.waitForHeightAriseAndTxPresent(topUpPayerId)

    val topUpSponsorId = sender.signedBroadcast(topUpSponsor.json()).id
    nodes.waitForHeightAriseAndTxPresent(topUpSponsorId)

    val sponsorshipId = sender.signedBroadcast(becomeSponsor.json()).id
    nodes.waitForHeightAriseAndTxPresent(sponsorshipId)

    sender.getSponsorship(payer.address) shouldBe SponsorshipInfo(List(sponsor.address))

    val paymentId = sender.signedBroadcast(makePayment.json()).id
    nodes.waitForHeightAriseAndTxPresent(paymentId)
    sender.assertBalances(payer.address, (10 - 4).lto)
    sender.assertBalances(recipient.address, (4).lto)
    sender.assertBalances(sponsor.address, (10 - 5 - 1).lto)

  }
}
object SponsorshipTransactionSuite {
  private val config =
    ConfigFactory.parseString(s"""
                                 |lto {
                                 |   blockchain.custom {
                                 |      functionality {
                                 |        pre-activated-features = {
                                 |          2 = 0
                                 |          3 = 0
                                 |          4 = 0
                                 |          5 = 0
                                 |          6 = 0
                                 |          7 = 0,
                                 |          10 = 0,
                                 |          11 = 0
                                 |        }
                                 |      }
                                 |   }
                                 |}""".stripMargin)
  val Configs: Seq[Config] = Default.map(config.withFallback(_)).take(4)
}
