package com.ltonetwork.state.diffs

import com.ltonetwork.account.{KeyTypes, PrivateKeyAccount}
import com.ltonetwork.block.TestBlock.{create => block}
import com.ltonetwork.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.transaction.certificate.CertificateTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CertificateTransactionDiffTest
  extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with WithDB {

  val fs: FunctionalitySettings = TestFunctionalitySettings.Enabled

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen(KeyTypes.SECP256R1)
    ts     <- positiveLongGen
    genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  property("certificate is stored or cleared correctly") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      certTx1 <- certificateTransactionGen.retryUntil(_.certificate.nonEmpty)
      certTx2 = CertificateTransaction.signed(3, ts + 20000, master, 10000000, Array.emptyByteArray).explicitGet()
    } yield (genesis, certTx1, certTx2)

    forAll(setup) { case (genesisTx, setTx, clearTx) =>
      val genesis = block(Seq(genesisTx))
      val setBlock = block(Seq(setTx))
      val clearBlock = block(Seq(clearTx))

      val sender = setTx.sender

      assertDiffAndState(Seq(genesis), setBlock, fs) {
        case (_, state) =>
          state.certificate(sender.toAddress) shouldBe Some(setTx.certificate)
      }

      assertDiffAndState(Seq(genesis, setBlock), clearBlock, fs) {
        case (_, state) =>
          state.certificate(sender.toAddress) shouldBe None
      }
    }
  }
}
