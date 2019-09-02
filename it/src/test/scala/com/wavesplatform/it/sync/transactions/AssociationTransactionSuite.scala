package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount}
import com.wavesplatform.api.http.assets.SignedTransferV1Request
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.AssociationTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.Base58
import org.scalatest.CancelAfterFailure
import play.api.libs.json._

import scala.concurrent.duration._
import com.wavesplatform.account.PublicKeyAccount._

class AssociationTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  val fee = 10^8
  test("invalid signed waves transfer should not be in UTX or blockchain") {

    val party = PrivateKeyAccount.fromSeed("party").explicitGet()

    val assocTx = AssociationTransaction.selfSigned(1,sender.privateKey,party.toAddress,42,None,fee,System.currentTimeMillis()).explicitGet()
    val transferId = sender
      .signedBroadcast(assocTx.json() + ("type" -> JsNumber(AssociationTransaction.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
    sender.getData()
  }

  test("can not make transfer without having enough effective balance") {
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    assertBadRequest(sender.transfer(secondAddress, firstAddress, secondEffBalance, minFee))
    nodes.waitForHeightArise()

    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }

  test("can not make transfer without having enough balance") {
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    assertBadRequestAndResponse(sender.transfer(secondAddress, firstAddress, secondBalance + 1.waves, minFee),
                                "Attempt to transfer unavailable funds")
    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }

}
