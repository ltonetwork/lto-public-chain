package com.ltonetwork.it.sync.smartcontract

import com.typesafe.config.Config
import com.ltonetwork.account.AddressOrAlias
import com.ltonetwork.crypto
import com.ltonetwork.it.NodeConfigs
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.sync._
import com.ltonetwork.it.transactions.BaseTransactionSuite
import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.state._
import com.ltonetwork.transaction.Proofs
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.dummyCompilerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber

/*
Scenario:
1. Alice's and swapBC1's balances initialisation
2. Create and setup smart contract for swapBC1
3. Alice funds swapBC1t
4. Alice can't take money from swapBC1
5.1 Bob takes funds because he knows secret hash and 5.2 after rollback wait height and Alice takes funds back
 */

class AtomicSwapSmartContractSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val BobBC1: String   = sender.createAddress()
  private val AliceBC1: String = sender.createAddress()
  private val swapBC1: String  = sender.createAddress()
  private val AlicesPK         = pkByAddress(AliceBC1)
  private val secretText       = "some secret message from Alice"
  private val shaSecret        = "BN6RTYGWcwektQfSFzH8raYo9awaLgQ7pLyWLQY4S4F5"

  /*
  One node because:
  1. There is an expected behavior of rollback and a possible issue with this. When the node are going rollback,
     there is a chance, it has a downloaded (but don't applied) extension. After the rollback, it applies an extension.
     This breaks the test.
  2. We have RollbackSuite
   */

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .buildNonConflicting()

  lazy val node = nodes.head

  test("step1: Balances initialization") {
    val toAliceBC1TxId = sender.transfer(sender.address, AliceBC1, 10 * transferAmount, minFee).id
    node.waitForTransaction(toAliceBC1TxId)

    val toSwapBC1TxId = sender.transfer(sender.address, swapBC1, minFee, minFee).id
    node.waitForTransaction(toSwapBC1TxId)
  }

  test("step2: Create and setup smart contract for swapBC1") {
    val beforeHeight = sender.height
    val sc1 = {
      val untyped = Parser(s"""
    let Bob = Address(base58'$BobBC1')
    let Alice = Address(base58'$AliceBC1')

    match tx {
      case ttx: TransferTransaction =>
        let txToBob = (ttx.recipient == Bob) && (sha256(ttx.proofs[0]) == base58'$shaSecret') && ((10 + $beforeHeight) >= height)
        let backToAliceAfterHeight = ((height >= (11 + $beforeHeight)) && (ttx.recipient == Alice))

        txToBob || backToAliceAfterHeight
      case other => false
    }""".stripMargin).get.value
      CompilerV1(dummyCompilerContext, untyped).explicitGet()._1
    }

    val pkSwapBC1 = pkByAddress(swapBC1)
    val script    = ScriptV1(sc1).explicitGet()
    val sc1SetTx = SetScriptTransaction
      .selfSigned(version = SetScriptTransaction.supportedVersions.head,
                  sender = pkSwapBC1,
                  script = Some(script),
                  fee = minFee,
                  timestamp = System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(sc1SetTx.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    node.waitForTransaction(setScriptId)

    val swapBC1ScriptInfo = sender.addressScriptInfo(swapBC1)

    swapBC1ScriptInfo.script.isEmpty shouldBe false
    swapBC1ScriptInfo.scriptText.isEmpty shouldBe false
  }

  test("step3: Alice makes transfer to swapBC1") {
    val txToSwapBC1 =
      TransferTransaction
        .selfSigned(
          version = 2,
          sender = pkByAddress(AliceBC1),
          recipient = AddressOrAlias.fromString(swapBC1).explicitGet(),
          amount = transferAmount + minFee + smartFee,
          timestamp = System.currentTimeMillis(),
          fee = minFee + smartFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(txToSwapBC1.json() + ("type" -> JsNumber(TransferTransaction.typeId.toInt)))
      .id
    node.waitForTransaction(transferId)
  }

  test("step4: Alice cannot make transfer from swapBC1 if height is incorrect") {
    val txToSwapBC1 =
      TransferTransaction
        .selfSigned(
          version = 2,
          sender = pkByAddress(swapBC1),
          recipient = AddressOrAlias.fromString(AliceBC1).explicitGet(),
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          fee = minFee + smartFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    assertBadRequest(sender.signedBroadcast(txToSwapBC1.json() + ("type" -> JsNumber(TransferTransaction.typeId.toInt))))
  }

  test("step5: Bob makes transfer; after revert Alice takes funds back") {
    val height = nodes.height.max

    val (bobBalance, bobEffBalance)     = notMiner.accountBalances(BobBC1)
    val (aliceBalance, aliceEffBalance) = notMiner.accountBalances(AliceBC1)
    val (swapBalance, swapEffBalance)   = notMiner.accountBalances(swapBC1)

    val unsigned =
      TransferTransaction
        .create(
          version = 2,
          chainId = None,
          sender = pkByAddress(swapBC1),
          recipient = AddressOrAlias.fromString(BobBC1).explicitGet(),
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          fee = minFee + smartFee,
          attachment = Array.emptyByteArray,
          sponsor = None,
          proofs = Proofs.empty
        )
        .explicitGet()

    val proof    = ByteStr(secretText.getBytes())
    val sigAlice = ByteStr(crypto.sign(AlicesPK, unsigned.bodyBytes()))
    val signed   = unsigned.copy(proofs = Proofs(Seq(proof, sigAlice)))

    nodes.waitForHeightArise()
    val versionedTransferId = sender.signedBroadcast(signed.json() + ("type" -> JsNumber(TransferTransaction.typeId.toInt))).id
    node.waitForTransaction(versionedTransferId)

    notMiner.assertBalances(swapBC1, swapBalance - transferAmount - (minFee + smartFee), swapEffBalance - transferAmount - (minFee + smartFee))
    notMiner.assertBalances(BobBC1, bobBalance + transferAmount, bobEffBalance + transferAmount)
    notMiner.assertBalances(AliceBC1, aliceBalance, aliceEffBalance)

    nodes.rollback(height, false)

    nodes.waitForHeight(height + 10)

    notMiner.accountBalances(swapBC1)
    assertNotFoundAndMessage(notMiner.transactionInfo(versionedTransferId), "Transaction is not in blockchain")

    val selfSignedToAlice = TransferTransaction
      .selfSigned(
        version = 2,
        sender = pkByAddress(swapBC1),
        recipient = AddressOrAlias.fromString(AliceBC1).explicitGet(),
        amount = transferAmount,
        timestamp = System.currentTimeMillis(),
        fee = minFee + smartFee,
        attachment = Array.emptyByteArray
      )
      .explicitGet()

    val transferToAlice =
      sender.signedBroadcast(selfSignedToAlice.json() + ("type" -> JsNumber(TransferTransaction.typeId.toInt))).id
    node.waitForTransaction(transferToAlice)

    notMiner.assertBalances(swapBC1, swapBalance - transferAmount - (minFee + smartFee), swapEffBalance - transferAmount - (minFee + smartFee))
    notMiner.assertBalances(BobBC1, bobBalance, bobEffBalance)
    notMiner.assertBalances(AliceBC1, aliceBalance + transferAmount, aliceEffBalance + transferAmount)
  }

}
