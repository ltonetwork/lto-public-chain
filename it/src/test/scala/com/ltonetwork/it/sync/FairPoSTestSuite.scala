package com.ltonetwork.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.ltonetwork.it.api.SyncHttpApi._
import com.ltonetwork.it.transactions.NodesFromDocker
import com.ltonetwork.it.util._
import org.scalatest.{CancelAfterFailure, FunSuite}

import scala.concurrent.duration._

class FairPoSTestSuite extends FunSuite with CancelAfterFailure with NodesFromDocker {
  import FairPoSTestSuite._

  private val transferFee    = 1.lto
  private val transferAmount = 1000.lto

  override protected def nodeConfigs: Seq[Config] = Configs

  test("blockchain grows with FairPoS activated") {
    nodes.head.waitForHeight(10, 3.minutes)

    val txId = nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, transferFee).id
    nodes.last.waitForTransaction(txId)

    val heightAfterTransfer = nodes.head.height

    nodes.head.waitForHeight(heightAfterTransfer + 20, 10.minutes)
  }
}

object FairPoSTestSuite {
  import com.ltonetwork.it.NodeConfigs._
  private val microblockActivationHeight = 0
  private val config =
    ConfigFactory.parseString(s"""
                                 |lto {
                                 |   blockchain.custom {
                                 |      functionality {
                                 |        pre-activated-features {1 = $microblockActivationHeight}
                                 |        generation-balance-depth-from-50-to-1000-after-height = 1000
                                 |      }
                                 |   }
                                 |   miner.quorum = 1
                                 |}""".stripMargin)
  val Configs: Seq[Config] = Default.map(config.withFallback(_)).take(4)
}
