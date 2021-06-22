package com.ltonetwork.mining

import com.ltonetwork.TransactionGen
import com.ltonetwork.lang.v1.compiler.Terms
import com.ltonetwork.state.{Blockchain, ByteStr, EitherExt2}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FreeSpec, Matchers}
import com.ltonetwork.account.{Address, PrivateKeyAccount}
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.transfer.TransferTransaction

class TxEstimatorsSuite extends FreeSpec with Matchers with PathMockFactory with TransactionGen {
  "scriptRunNumber" - {
    "smart account" - {
      "should not count transactions going from a regular account" in {
        val blockchain = stub[Blockchain]
        (blockchain.hasScript _).when(*).onCall((_: Address) => false).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferLtoTx) shouldBe 0
      }

      "should count transactions going from a smart account" in {
        val blockchain = stub[Blockchain]
        (blockchain.hasScript _).when(*).onCall((_: Address) => true).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferLtoTx) shouldBe 1
      }
    }

  }

  private val assetId = ByteStr("coin_id".getBytes())
  private val script  = ScriptV1(Terms.TRUE, checkSize = false).explicitGet()

  private val transferLtoTx = TransferTransaction
    .selfSigned(
      version = 1,
      sender = PrivateKeyAccount("sender".getBytes()),
      recipient = PrivateKeyAccount("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      fee = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val transferAssetsTx = TransferTransaction
    .selfSigned(
      version = 1,
      sender = PrivateKeyAccount("sender".getBytes()),
      recipient = PrivateKeyAccount("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      fee = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

}
