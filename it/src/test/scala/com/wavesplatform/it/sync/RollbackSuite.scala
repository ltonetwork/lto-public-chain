package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{NodeConfigs, TransferSending}
import com.wavesplatform.state.{BooleanDataEntry, IntegerDataEntry}
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class RollbackSuite extends FunSuite with CancelAfterFailure with TransferSending with NodesFromDocker with Matchers {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(1, _.nonMiner)
      .buildNonConflicting()

  private val nodeAddresses = nodeConfigs.map(_.getString("address")).toSet
  private def sender        = nodes.last
  private def firstAddress  = sender.address

  test("Apply the same transfer transactions twice with return to UTX") {

    val startHeight = sender.height

    Await.result(processRequests(generateTransfersToRandomAddresses(190, nodeAddresses)), 2.minutes)

    nodes.waitFor[Int]("empty utx")(1.second)(_.utxSize, _.forall(_ == 0))

    nodes.waitForHeightArise()

    val stateAfterFirstTry = nodes.head.debugStateAt(sender.height)

    nodes.rollback(startHeight)

    nodes.waitFor[Int]("empty utx")(1.second)(_.utxSize, _.forall(_ == 0))

    nodes.waitForHeightArise()

    val stateAfterSecondTry = nodes.head.debugStateAt(sender.height)

    assert(stateAfterSecondTry.size == stateAfterFirstTry.size)

    stateAfterSecondTry should contain theSameElementsAs stateAfterFirstTry
  }

  test("Just rollback transactions") {
    val startHeight      = sender.height
    val stateBeforeApply = sender.debugStateAt(startHeight)

    val requests = generateTransfersToRandomAddresses(190, nodeAddresses)
    Await.result(processRequests(requests), 2.minutes)

    nodes.waitFor[Int]("empty utx")(1.second)(_.utxSize, _.forall(_ == 0))

    nodes.waitForHeightArise()

    sender.debugStateAt(sender.height).size shouldBe stateBeforeApply.size + 190

    nodes.rollback(startHeight, returnToUTX = false)

    nodes.waitFor[Int]("empty utx")(1.second)(_.utxSize, _.forall(_ == 0))

    nodes.waitForHeightArise()

    val stateAfterApply = sender.debugStateAt(sender.height)

    stateAfterApply should contain theSameElementsAs stateBeforeApply

  }

  test("Data transaction rollback") {
    val node       = nodes.head
    val entry1     = IntegerDataEntry("1", 0)
    val entry2     = BooleanDataEntry("2", true)
    val entry3     = IntegerDataEntry("1", 1)
    val txsBefore0 = sender.transactionsByAddress(firstAddress, 10)

    val tx1 = sender.putData(firstAddress, List(entry1), calcDataFee(List(entry1))).id
    nodes.waitForHeightAriseAndTxPresent(tx1)
    val txsBefore1 = sender.transactionsByAddress(firstAddress, 10)

    val tx1height = sender.waitForTransaction(tx1).height

    val tx2 = sender.putData(firstAddress, List(entry2, entry3), calcDataFee(List(entry2, entry3))).id
    nodes.waitForHeightAriseAndTxPresent(tx2)

    val data2 = sender.getData(firstAddress)
    assert(data2 == List(entry3, entry2))

    nodes.rollback(tx1height, returnToUTX = false)
    nodes.waitForSameBlockHeadesAt(tx1height)

    val data1 = node.getData(firstAddress)
    assert(data1 == List(entry1))
    sender.transactionsByAddress(firstAddress, 10) should contain theSameElementsAs txsBefore1

    nodes.rollback(tx1height - 1, returnToUTX = false)
    nodes.waitForSameBlockHeadesAt(tx1height - 1)

    val data0 = node.getData(firstAddress)
    assert(data0 == List.empty)
    sender.transactionsByAddress(firstAddress, 10) should contain theSameElementsAs txsBefore0
  }
}
