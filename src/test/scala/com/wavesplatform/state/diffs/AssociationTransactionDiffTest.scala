package com.wavesplatform.state.diffs

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.state.{Blockchain, EitherExt2}
import com.wavesplatform.transaction.{AssociationTransaction, GenesisTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class AssociationTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  property("can find outgoing assoc") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      party <- accountGen
      feeOverhead           <- Gen.choose[Long](0, ENOUGH_AMT)
      version               <- Gen.oneOf(AssociationTransaction.supportedVersions.toSeq)
      tx = AssociationTransaction.selfSigned(version, master, party,42, None, 10^8 + feeOverhead, ts + 10000).explicitGet()
    } yield (genesis, tx)

    forAll(setup) {
      case (genesis, assoc) =>
        assertDiffAndState(Seq(block(Seq(genesis))), block(Seq(assoc))) {
          case (d,b) => b.associations(genesis.recipient) shouldBe Blockchain.Associations(List((2,assoc)),List.empty)
        }
    }
  }
}
