package com.wavesplatform.mining

import com.wavesplatform.TransactionGen
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.{Blockchain, ByteStr, EitherExt2}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FreeSpec, Matchers}
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.transaction.transfer.TransferTransactionV1

class TxEstimatorsSuite extends FreeSpec with Matchers with PathMockFactory with TransactionGen {
  "scriptRunNumber" - {
    "smart account" - {
      "should not count transactions going from a regular account" in {
        val blockchain = stub[Blockchain]
        (blockchain.hasScript _).when(*).onCall((_: Address) => false).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferWavesTx) shouldBe 0
      }

      "should count transactions going from a smart account" in {
        val blockchain = stub[Blockchain]
        (blockchain.hasScript _).when(*).onCall((_: Address) => true).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferWavesTx) shouldBe 1
      }
    }

  }

  private val assetId = ByteStr("coin_id".getBytes())
  private val script  = ScriptV1(Terms.TRUE, checkSize = false).explicitGet()

  private val transferWavesTx = TransferTransactionV1
    .selfSigned(
      sender = PrivateKeyAccount("sender".getBytes()),
      recipient = PrivateKeyAccount("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      feeAmount = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val transferAssetsTx = TransferTransactionV1
    .selfSigned(
      sender = PrivateKeyAccount("sender".getBytes()),
      recipient = PrivateKeyAccount("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      feeAmount = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

}
