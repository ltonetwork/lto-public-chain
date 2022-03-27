package com.ltonetwork.state.diffs

import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.block.TestBlock.{create => block}
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class RegisterTransactionDiffTest
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with WithDB {

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  def register(version: Byte, sender: PrivateKeyAccount, data: List[PublicKeyAccount], fee: Long, timestamp: Long): RegisterTransaction =
    RegisterTransaction.signed(version, timestamp, sender, fee, data).explicitGet()

  property("cannot overspend funds") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      value                 <- accountGen
      feeOverhead           <- Gen.choose[Long](1, ENOUGH_AMT)
      version               <- Gen.oneOf(RegisterTransaction.supportedVersions.toSeq)
      registerTx = register(version, master, List(value), ENOUGH_AMT + feeOverhead, ts + 10000)
    } yield (genesis, registerTx)

    forAll(setup) {
      case (genesis, registerTx) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(registerTx))) { blockDiffEi =>
          blockDiffEi should produce("negative lto balance")
        }
    }
  }
}
