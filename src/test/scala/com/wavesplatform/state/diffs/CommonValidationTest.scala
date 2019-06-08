package com.wavesplatform.state.diffs

import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class CommonValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with WithState with NoShrink {

//  def preconditionsAndTransactions(transferFee:Long, leaseFee: Long, leaseCancelFee: Long): Gen[(GenesisTransaction, TransferTransaction)] = for {
//    master    <- accountGen
//    acc <- otherAccountGen(candidate = master)
//    ts        <- positiveIntGen
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
//   funding  = TransferTransactionV2.selfSigned(2,master,acc, ENOUGH_AMT/2,ts, 100*1000*1000,Array.emptyByteArray )
//  } yield (genesis, funding)


  property("disallows double spending") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, TransferTransactionV1)] = for {
      master    <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      transfer: TransferTransactionV1 <- wavesTransferGeneratorP(master, recipient)
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
