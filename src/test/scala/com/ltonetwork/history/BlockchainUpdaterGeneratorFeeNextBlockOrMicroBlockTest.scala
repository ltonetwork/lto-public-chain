package com.ltonetwork.history

import com.ltonetwork.TransactionGen
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.state._
import com.ltonetwork.state.diffs._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import com.ltonetwork.transaction.GenesisTransaction
import com.ltonetwork.transaction.transfer._

class BlockchainUpdaterGeneratorFeeNextBlockOrMicroBlockTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen {

  type Setup = (GenesisTransaction, TransferTransactionV1, TransferTransactionV1, TransferTransactionV1)

  val preconditionsAndPayments: Gen[Setup] = for {
    sender    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis: GenesisTransaction        = GenesisTransaction.create(sender, ENOUGH_AMT, ts).explicitGet()
    somePayment: TransferTransactionV1 = createLtoTransfer(sender, recipient, 1, 100 * 1000 * 1000, ts + 1).explicitGet()
    // generator has enough balance for this transaction if gets fee for block before applying it
    generatorPaymentOnFee: TransferTransactionV1 = createLtoTransfer(defaultSigner, recipient, 11, 100 * 1000 * 1000, ts + 2).explicitGet()
    someOtherPayment: TransferTransactionV1      = createLtoTransfer(sender, recipient, 1, 100 * 1000 * 1000, ts + 3).explicitGet()
  } yield (genesis, somePayment, generatorPaymentOnFee, someOtherPayment)

  property("generator should get fees before applying block in block + micro") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0LtoSettings) {
      case (domain, (genesis, somePayment, generatorPaymentOnFee, someOtherPayment)) =>
        val (block, microBlocks) =
          chainBaseAndMicro(randomSig, genesis, Seq(Seq(somePayment), Seq(generatorPaymentOnFee, someOtherPayment)), genesis.timestamp)
        domain.blockchainUpdater.processBlock(block).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microBlocks(0)) shouldBe 'right
        domain.blockchainUpdater.processMicroBlock(microBlocks(1)) should produce("unavailable funds")
    }
  }

  property("generator should get fees after applying every transaction in two blocks") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0LtoSettings) {
      case (domain, (genesis, somePayment, generatorPaymentOnFee, someOtherPayment)) =>
        val blocks = chainBlocks(Seq(Seq(genesis, somePayment), Seq(generatorPaymentOnFee, someOtherPayment)))
        domain.blockchainUpdater.processBlock(blocks(0)) shouldBe 'right
        domain.blockchainUpdater.processBlock(blocks(1)) should produce("unavailable funds")
    }
  }

  property("generator should get fees after applying every transaction in block + micro") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0LtoSettings) {
      case (domain, (genesis, somePayment, generatorPaymentOnFee, someOtherPayment)) =>
        val (block, microBlocks) =
          chainBaseAndMicro(randomSig, genesis, Seq(Seq(somePayment), Seq(generatorPaymentOnFee, someOtherPayment)), genesis.timestamp)
        domain.blockchainUpdater.processBlock(block).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microBlocks(0)).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microBlocks(1)) should produce("unavailable funds")
    }
  }
}
