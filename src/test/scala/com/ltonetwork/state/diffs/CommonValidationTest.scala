package com.ltonetwork.state.diffs

import com.ltonetwork.block.TestBlock
import com.ltonetwork.db.WithState
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class CommonValidationTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with WithState with NoShrink {

//  def preconditionsAndTransactions(transferFee:Long, leaseFee: Long, leaseCancelFee: Long): Gen[(GenesisTransaction, TransferTransaction)] = for {
//    master    <- accountGen
//    acc <- otherAccountGen(candidate = master)
//    ts        <- positiveIntGen
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
//   funding  = TransferTransaction.selfSigned(2,master,acc, ENOUGH_AMT/2,ts, 100*1000*1000,Array.emptyByteArray )
//  } yield (genesis, funding)

  property("disallows double spending") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, TransferTransaction)] = for {
      master    <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      transfer: TransferTransaction <- ltoTransferGeneratorP(master, recipient)
    } yield (genesis, transfer)

    forAll(preconditionsAndPayment) {
      case (genesis, transfer) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, transfer))), TestBlock.create(Seq(transfer))) { blockDiffEi =>
          blockDiffEi should produce("AlreadyInTheState")
        }

        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer, transfer))) { blockDiffEi =>
          blockDiffEi should produce("AlreadyInTheState")
        }
    }
  }

}
