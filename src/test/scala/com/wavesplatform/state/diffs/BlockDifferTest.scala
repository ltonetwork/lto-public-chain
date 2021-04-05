package com.wavesplatform.state.diffs

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.BlockGen
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.block.Block
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{Blockchain, Diff, EitherExt2}
import com.wavesplatform.transaction.GenesisTransaction
import org.scalatest.{FreeSpecLike, Matchers}
import scorex.crypto.signatures.Curve25519._

class BlockDifferTest extends FreeSpecLike with Matchers with BlockGen with WithState {

  private val TransactionFee = 100000000L

  def randomPrivateKeyAccount(): PrivateKeyAccount = {
    val seed = Array.ofDim[Byte](KeyLength)
    ThreadLocalRandom.current().nextBytes(seed)
    PrivateKeyAccount(seed)
  }

  private val signerA, signerB = randomPrivateKeyAccount()

  private val testChain: Seq[Block] = {
    val master, recipient = randomPrivateKeyAccount()
    getTwoMinersBlockChain(master, recipient, 9)
  }

  "BlockDiffer" - {
    "NG fees calculation" - {

      /*
      | N | fee | signer | A receive | A balance | B receive | B balance |
      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
      |1  |0    |A       |0          |0          |0          |0          | <- genesis
      |2  |10   |B       |0          |0          |10         |+10        |  b: 0.4
      |3  |10   |A       |10         |+10        |0          |0          |  a: 1
      |4  |10   |B       |0          |10         |+10        |10+10=20   |  b: 1.4
      |5  |10   |A       |4          |10+4=14    |0          |20         |  a: 2
      |6  |10   |B       |0          |14         |+4+6=10    |20+10=30   |  b: 2.4
      |7  |10   |A       |4+6=10     |14+10=24   |0          |30         |  a: 3
      |8  |10   |B       |0          |24         |+4+6=10    |30+10=40   |  b: 3.4
      |9  |10   |A       |4+6=10     |24+10=34   |0          |40         |  a:4        <- 1st check
      |10 |10   |B       |0          |34         |+4+6=10    |40+10=50   |  b: 4.4     <- 2nd check
       */
      "height > enableMicroblocksAfterHeight - a miner should receive 60% of previous block's fee and 40% of the current one" in {
        assertDiff(testChain.init) {
          case (_, s) =>
            s.portfolio(signerA).balance shouldBe 400000000
        }

        assertDiff(testChain) {
          case (_, s) =>
            s.portfolio(signerB).balance shouldBe 440000000
        }
      }
    }
  }

  private def assertDiff(blocks: Seq[Block])(assertion: (Diff, Blockchain) => Unit): Unit = {
    val fs = FunctionalitySettings(
      featureCheckBlocksPeriod = 42,
      blocksForFeatureActivation = 1,
      preActivatedFeatures = Map.empty,
      doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    )
    assertDiffAndState(blocks.init, blocks.last, fs)(assertion)
  }

  private def getTwoMinersBlockChain(from: PrivateKeyAccount, to: PrivateKeyAccount, numPayments: Int): Seq[Block] = {
    val ts                   = System.currentTimeMillis() - 100000
    val genesisTx            = GenesisTransaction.create(from, Long.MaxValue - 1, ts).explicitGet()
    val features: Set[Short] = Set[Short](2)

    val paymentTxs = (1 to numPayments).map { i =>
      createWavesTransfer(
        from,
        to,
        amount = 10000,
        TransactionFee,
        timestamp = ts + i * 1000
      ).explicitGet()
    }

    (genesisTx +: paymentTxs).zipWithIndex.map {
      case (x, i) =>
        val signer = if (i % 2 == 0) signerA else signerB
        TestBlock.create(signer, Seq(x), features)
    }
  }
}
