package com.wavesplatform.it.sync.transactions

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.account.PublicKeyAccount._
import com.wavesplatform.api.http.SponsorshipApiRoute.SponsorshipInfo
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.SponsorshipTransaction
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import org.scalatest.CancelAfterFailure

class SponsorshipTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  val fee = 1.waves

  val payer     = PrivateKeyAccount.fromSeed("sender").explicitGet()
  val recipient = PrivateKeyAccount.fromSeed("recipient").explicitGet()
  val sponsor   = PrivateKeyAccount.fromSeed("sponsor").explicitGet()

  override protected def nodeConfigs: Seq[Config] = SponsorshipTransactionSuite.Configs
  
  test("sponsor pays for sender") {

    val topUpPayer =
      TransferTransactionV2.selfSigned(2, sender.privateKey, payer, 10.waves, System.currentTimeMillis(), fee, Array.emptyByteArray).explicitGet()
    val topUpSponsor =
      TransferTransactionV2.selfSigned(2, sender.privateKey, sponsor, 10.waves, System.currentTimeMillis(), fee, Array.emptyByteArray).explicitGet()
    val becomeSponsor = SponsorshipTransaction.selfSigned(1, sponsor, payer, 5*fee, System.currentTimeMillis()).explicitGet()
    val makePayment =
      TransferTransactionV2.selfSigned(2, payer, recipient, 4.waves, System.currentTimeMillis(), fee, Array.emptyByteArray).explicitGet()

    val topUpPayerId = sender.signedBroadcast(topUpPayer.json()).id
    nodes.waitForHeightAriseAndTxPresent(topUpPayerId)

    val topUpSponsorId = sender.signedBroadcast(topUpSponsor.json()).id
    nodes.waitForHeightAriseAndTxPresent(topUpSponsorId)

    val sponsorshipId = sender.signedBroadcast(becomeSponsor.json()).id
    nodes.waitForHeightAriseAndTxPresent(sponsorshipId)

    sender.getSponsorship(payer.address) shouldBe SponsorshipInfo(Some(sponsor.address))

    val paymentId = sender.signedBroadcast(makePayment.json()).id
    nodes.waitForHeightAriseAndTxPresent(paymentId)
    sender.assertBalances(payer.address, (10 - 1 - 4).waves)
    sender.assertBalances(recipient.address, (10 + 4).waves)
    sender.assertBalances(sponsor.address, (10 - 5 - 1).waves)

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