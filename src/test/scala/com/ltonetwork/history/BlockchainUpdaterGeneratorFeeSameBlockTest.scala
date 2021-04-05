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

class BlockchainUpdaterGeneratorFeeSameBlockTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen {

  type Setup = (GenesisTransaction, TransferTransactionV1, TransferTransactionV1)

  val preconditionsAndPayments: Gen[Setup] = for {
    sender    <- accountGen
    recipient <- accountGen
    fee       <- smallFeeGen
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(sender, ENOUGH_AMT, ts).explicitGet()
    payment: TransferTransactionV1 <- wavesTransferGeneratorP(sender, recipient)
    generatorPaymentOnFee: TransferTransactionV1 = createWavesTransfer(defaultSigner, recipient, payment.fee, fee, ts + 1).explicitGet()
  } yield (genesis, payment, generatorPaymentOnFee)

  property("block generator can't spend fee after transaction") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, somePayment, generatorPaymentOnFee)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), Seq(generatorPaymentOnFee, somePayment)))
        blocks.init.foreach(block => domain.blockchainUpdater.processBlock(block).explicitGet())
        domain.blockchainUpdater.processBlock(blocks.last) should produce("unavailable funds")
    }
  }
}
