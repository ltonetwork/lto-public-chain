package com.ltonetwork.it.sync.smartcontract

import com.ltonetwork.crypto
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.sync.{minFee, transferAmount}
import com.ltonetwork.it.transactions.BaseTransactionSuite
import com.ltonetwork.it.util._
import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.state._
import com.ltonetwork.transaction.Proofs
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.dummyCompilerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.{JsNumber, Json}

class SetScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val fourthAddress: String = sender.createAddress()

  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)
  private val acc3 = pkByAddress(fourthAddress)

  test("setup acc0 with 1 lto") {
    val tx =
      TransferTransaction
        .signed(
          version = 2,
          sender = sender.privateKey,
          recipient = acc0,
          amount = 3 * transferAmount + 3 * (0.00001.lto + 0.00002.lto), // Script fee
          timestamp = System.currentTimeMillis(),
          fee = minFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransaction.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("set acc0 as 2of2 multisig") {
    val scriptText = {
      val untyped = Parser(s"""

        let A = base58'${ByteStr(acc1.publicKey)}'
        let B = base58'${ByteStr(acc2.publicKey)}'

        let AC = sigVerify(tx.bodyBytes,tx.proofs[0],A)
        let BC = sigVerify(tx.bodyBytes,tx.proofs[1],B)

         AC && BC

      """.stripMargin).get.value
      CompilerV1(dummyCompilerContext, untyped).explicitGet()._1
    }

    val script = ScriptV1(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .signed(1, System.currentTimeMillis(), acc0, minFee, Some(script))
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val acc0ScriptInfo = sender.addressScriptInfo(acc0.address)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true

    val json = Json.parse(sender.get(s"/transactions/info/$setScriptId").getResponseBody)
    (json \ "script").as[String].startsWith("base64:") shouldBe true
  }

  test("can't send from acc0 using old pk") {
    val tx =
      TransferTransaction
        .signed(
          version = 2,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          fee = minFee + 0.00001.lto + 0.00002.lto,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    assertBadRequest(sender.signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransaction.typeId.toInt))))
  }

  test("can send from acc0 using multisig of acc1 and acc2") {
    val unsigned =
      TransferTransaction
        .create(
          version = 2,
          chainId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          fee = minFee + 0.004.lto,
          attachment = Array.emptyByteArray,
          sponsor = None,
          proofs = Proofs.empty
        )
        .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val signed = unsigned.copy(proofs = Proofs(Seq(sig1, sig2)))

    val versionedTransferId =
      sender.signedBroadcast(signed.json() + ("type" -> JsNumber(TransferTransaction.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(versionedTransferId)
  }

  test("can clear script at acc0") {
    val unsigned = SetScriptTransaction
      .create(
        version = SetScriptTransaction.supportedVersions.head,
        chainId = None,
        sender = acc0,
        script = None,
        fee = minFee + 0.004.lto,
        timestamp = System.currentTimeMillis(),
        sponsor = None,
        proofs = Proofs.empty
      )
      .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val signed = unsigned.copy(proofs = Proofs(Seq(sig1, sig2)))
    val clearScriptId = sender
      .signedBroadcast(signed.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(clearScriptId)
  }

  test("can send using old pk of acc0") {
    val tx =
      TransferTransaction
        .signed(
          version = 2,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          fee = minFee + 0.004.lto,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    val txId = sender.signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransaction.typeId.toInt))).id
    nodes.waitForHeightAriseAndTxPresent(txId)
  }
}
