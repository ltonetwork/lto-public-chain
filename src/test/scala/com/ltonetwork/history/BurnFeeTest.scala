package com.ltonetwork.history

import com.ltonetwork.TransactionGen
import com.ltonetwork.account.Address
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.settings.LtoSettings
import com.ltonetwork.state._
import com.ltonetwork.state.diffs.{BlockDiffer, ENOUGH_AMT}
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.transfer.TransferTransaction
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class BurnFeeTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {
  type Setup = (GenesisTransaction, TransferTransaction)

  val burnFeeEnabledSettings: LtoSettings = settings.copy(
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
    fee    <- Gen.choose(150000000L, 150000000L)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    tx                          = TransferTransaction.signed(1, ts, master, fee, alice, amount, ByteStr.empty.arr).explicitGet()
  } yield (genesis, tx)

  property("burns exactly 0.1 LTO for 1 transaction") {

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
