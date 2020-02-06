package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.account.PublicKeyAccount._
import com.wavesplatform.api.http.SponsorshipApiRoute.SponsorshipInfo
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

  test("sponsor pays for sender") {

    val topUpPayer =
      TransferTransactionV2.selfSigned(2, notMiner.privateKey, payer, 10.waves, System.currentTimeMillis(), fee, Array.emptyByteArray).explicitGet()
    val topUpSponsor =
      TransferTransactionV2.selfSigned(2, notMiner.privateKey, sponsor, 10.waves, System.currentTimeMillis(), fee, Array.emptyByteArray).explicitGet()
    val becomeSponsor = SponsorshipTransaction.selfSigned(1, sponsor, payer, fee, System.currentTimeMillis()).explicitGet()
    val makePayment =
      TransferTransactionV2.selfSigned(2, payer, recipient, 4.waves, System.currentTimeMillis(), fee, Array.emptyByteArray).explicitGet()

    val topUpPayerId = notMiner.signedBroadcast(topUpPayer.json()).id
    nodes.waitForHeightAriseAndTxPresent(topUpPayerId)

    val topUpSponsorId = notMiner.signedBroadcast(topUpSponsor.json()).id
    nodes.waitForHeightAriseAndTxPresent(topUpSponsorId)

    val sponsorshipId = notMiner.signedBroadcast(becomeSponsor.json()).id
    nodes.waitForHeightAriseAndTxPresent(sponsorshipId)

    notMiner.getSponsorship(payer.address) shouldBe SponsorshipInfo(Some(sponsor.address))

    val paymentId = notMiner.signedBroadcast(makePayment.json()).id
    nodes.waitForHeightAriseAndTxPresent(paymentId)
    notMiner.assertBalances(payer.address, (10 - 1 - 4).waves)
    notMiner.assertBalances(recipient.address, (10 + 4).waves)
    notMiner.assertBalances(sponsor.address, (10 - 5 - 1).waves)

  }
}
