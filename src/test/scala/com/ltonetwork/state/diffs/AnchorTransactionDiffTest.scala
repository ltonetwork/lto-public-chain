package com.ltonetwork.state.diffs

import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.block.TestBlock.{create => block}
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class AnchorTransactionDiffTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  def anchor(version: Byte, sender: PrivateKeyAccount, data: List[ByteStr], fee: Long, timestamp: Long): AnchorTransaction =
    AnchorTransaction.signed(version, timestamp, sender, fee, data).explicitGet()

  property("cannot overspend funds") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      value                 <- bytes64gen
      feeOverhead           <- Gen.choose[Long](1, ENOUGH_AMT)
      version               <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      anchorTx = anchor(version, master, List(ByteStr(value)), ENOUGH_AMT + feeOverhead, ts + 10000)
    } yield (genesis, anchorTx)

    forAll(setup) {
      case (genesis, anchorTx) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(anchorTx))) { blockDiffEi =>
          blockDiffEi should produce("negative lto balance")
        }
    }
  }
}
