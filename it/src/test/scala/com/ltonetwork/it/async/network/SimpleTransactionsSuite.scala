package com.ltonetwork.it.async.network

import java.nio.charset.StandardCharsets

import com.typesafe.config.Config
import com.ltonetwork.account.Address
import com.ltonetwork.it._
import com.ltonetwork.it.api.AsyncHttpApi._
import com.ltonetwork.it.api.AsyncNetworkApi._
import com.ltonetwork.it.api._
import com.ltonetwork.it.transactions.BaseTransactionSuite
import com.ltonetwork.network.{RawBytes, TransactionSpec}
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.transaction.transfer._
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.concurrent.Await
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.language.postfixOps

class SimpleTransactionsSuite extends BaseTransactionSuite with Matchers with ScalaFutures with IntegrationPatience with RecoverMethods {

  private val waitCompletion = 2.minutes

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(2))
      .withDefault(3)
      .build()

  private def node = nodes.head

  test("valid tx send by network to node should be in blockchain") {
    val tx = TransferTransactionV1
      .selfSigned(node.privateKey, Address.fromString(node.address).explicitGet(), 1L, System.currentTimeMillis(), 100000000L, Array())
      .right
      .get
    val f = for {
      _          <- node.sendByNetwork(RawBytes.from(tx))
      txFromNode <- node.waitForTransaction(tx.id().base58)
    } yield {
      txFromNode.id shouldBe tx.id().toString
    }
    Await.result(f, waitCompletion)
  }

  test("invalid tx send by network to node should be not in UTX or blockchain") {
    val tx = TransferTransactionV1
      .selfSigned(node.privateKey,
                  Address.fromString(node.address).explicitGet(),
                  1L,
                  System.currentTimeMillis() + (1 days).toMillis,
                  100000L,
                  Array())
      .right
      .get
    val f = for {
      _         <- node.sendByNetwork(RawBytes.from(tx))
      maxHeight <- traverse(nodes)(_.height).map(_.max)
      _         <- traverse(nodes)(_.waitForHeight(maxHeight + 1))
      _         <- traverse(nodes)(_.ensureTxDoesntExist(tx.id().base58))
    } yield ()
    Await.result(f, waitCompletion)
  }

  test("should blacklist senders of non-parsable transactions") {
    val f = for {
      blacklistBefore <- node.blacklistedPeers
      _               <- node.sendByNetwork(RawBytes(TransactionSpec.messageCode, "foobar".getBytes(StandardCharsets.UTF_8)))
      _ <- node.waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers > ${blacklistBefore.size}")(_.blacklistedPeers,
                                                                                             _.lengthCompare(blacklistBefore.size) > 0,
                                                                                             500.millis)
    } yield ()
    Await.result(f, waitCompletion)
  }
}
