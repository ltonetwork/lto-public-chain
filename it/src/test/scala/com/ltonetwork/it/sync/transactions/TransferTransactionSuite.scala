package com.ltonetwork.it.sync.transactions

import com.ltonetwork.account.Address
import com.ltonetwork.api.http.requests.TransferRequest
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.sync._
import com.ltonetwork.it.transactions.BaseTransactionSuite
import com.ltonetwork.it.util._
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.Base58
import org.scalatest.CancelAfterFailure
import play.api.libs.json._

import scala.concurrent.duration._

class TransferTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  test("lto transfer changes lto balances and eff.b.") {
    val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    val transferId = sender.transfer(firstAddress, secondAddress, transferAmount, minFee).id

    nodes.waitForHeightAriseAndTxPresent(transferId)

    notMiner.assertBalances(firstAddress, firstBalance - transferAmount - minFee, firstEffBalance - transferAmount - minFee)
    notMiner.assertBalances(secondAddress, secondBalance + transferAmount, secondEffBalance + transferAmount)
  }

  test("invalid signed lto transfer should not be in UTX or blockchain") {
    def invalidTx(timestamp: Long = System.currentTimeMillis, fee: Long = com.ltonetwork.it.STD_FEE) =
      TransferTransaction
        .selfSigned(1, timestamp, sender.privateKey, fee, Address.fromString(sender.address).explicitGet(), 1, Array.emptyByteArray)
        .right
        .get

    def request(tx: TransferTransaction): TransferRequest =
      TransferRequest(
        senderPublicKey = Some(Base58.encode(tx.sender.publicKey)),
        recipient = tx.recipient.stringRepr,
        amount = tx.amount,
        fee = tx.fee,
        timestamp = Some(tx.timestamp),
        attachment = Some(ByteStr(tx.attachment)),
        signature = Some(tx.signature)
      )

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val invalidTxs = Seq(
      (invalidTx(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction .* is from far future"),
      (invalidTx(fee = 99999), "Fee .* does not exceed minimal value")
    )

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(request(tx)), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().base58))
    }

    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance1, eff1)

  }

  test("can not make transfer without having enough effective balance") {
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    assertBadRequest(sender.transfer(secondAddress, firstAddress, secondEffBalance, minFee))
    nodes.waitForHeightArise()

    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }

  test("can not make transfer without having enough balance") {
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    assertBadRequestAndResponse(sender.transfer(secondAddress, firstAddress, secondBalance + 1.lto, minFee),
                                "Attempt to transfer unavailable funds")
    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }

}
