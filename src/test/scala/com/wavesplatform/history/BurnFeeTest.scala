package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.{BlockDiffer, ENOUGH_AMT}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class BurnFeeTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {
  type Setup = (GenesisTransaction, TransferTransactionV1)

  val burnFeeEnabledSettings: WavesSettings = settings.copy(
    blockchainSettings =
      DefaultBlockchainSettings.copy(functionalitySettings =
        DefaultBlockchainSettings.functionalitySettings.copy(
          preActivatedFeatures = Map(
            BlockchainFeatures.BurnFeeture.id -> 0
          ))))

  val preconditionsAndPayments: Gen[Setup] = for {
    master <- accountGen
    alice  <- accountGen
    ts     <- positiveIntGen
    amount <- Gen.choose(1000000000L, 100000000000L)
    fee    <- Gen.choose(100000000L, 10000000000L)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    tx                          = TransferTransactionV1.selfSigned(master, alice, amount, ts, fee, ByteStr.empty.arr).explicitGet()
  } yield (genesis, tx)

  property("all txs in different blocks: B0 <- B1 <- B2 <- B3!") {

    scenario(preconditionsAndPayments, burnFeeEnabledSettings) {
      case (domain, (genesis, masterToAlice)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), Seq(masterToAlice)))
        domain.blockchainUpdater.processBlock(blocks(0)).explicitGet()
        domain.blockchainUpdater.processBlock(blocks(1)).explicitGet()
        domain.blockchainUpdater.processBlock(buildBlockOfTxs(blocks(1).uniqueId, Seq.empty, genesis.timestamp)).explicitGet()

        val masterBalance = domain.blockchainUpdater.balance(masterToAlice.sender)
        val aliceBalance  = domain.blockchainUpdater.balance(masterToAlice.recipient.asInstanceOf[Address])
        val minerBalance  = domain.blockchainUpdater.balance(defaultSigner.toAddress)
        masterBalance shouldBe (genesis.amount - masterToAlice.amount - masterToAlice.fee)
        aliceBalance shouldBe masterToAlice.amount
        minerBalance shouldBe (masterToAlice.fee - BlockDiffer.feeBurnAmt)
        (masterBalance + aliceBalance + minerBalance) shouldBe (genesis.amount - BlockDiffer.feeBurnAmt)
    }
  }

}
