package com.ltonetwork.state.diffs

import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.block.TestBlock
import com.ltonetwork.block.TestBlock.{create => block}
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.settings.TestFunctionalitySettings
import com.ltonetwork.state.{EitherExt2, LeaseBalance, Portfolio}
import com.ltonetwork.transaction.burn.BurnTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.{NoShrink, TransactionGen, WithDB}
import com.ltonetwork.utils.DoubleExt
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BurnTransactionDiffTest
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with WithDB {

  private val features = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.Juicy.id -> 0)
  private val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = features)

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  def burn(version: Byte, sender: PrivateKeyAccount, amount: Long, fee: Long, timestamp: Long): BurnTransaction =
    BurnTransaction.signed(version, timestamp, sender, fee, amount).explicitGet()

  property("burn lto") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      version               <- Gen.oneOf(BurnTransaction.supportedVersions.toSeq)
      fee                   <- smallFeeGen
      amount                <- Gen.choose[Long](1, ENOUGH_AMT / 10)
      burnTx = burn(version, master, amount, fee, ts + 10000)
    } yield (genesis, burnTx)
    val effectiveFee = 1.lto

    forAll(setup) {
      case (genesis, burnTx) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(burnTx)), fs) {
          case (totalDiff, newState) =>
            val senderPortfolio = newState.portfolio(burnTx.sender)
            senderPortfolio shouldBe Portfolio(ENOUGH_AMT - (burnTx.amount + effectiveFee), LeaseBalance.empty)
            newState.burned shouldBe burnTx.amount + (effectiveFee / 2)
        }
    }
  }

  property("cannot overspend funds") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      fee                   <- smallFeeGen
      version               <- Gen.oneOf(BurnTransaction.supportedVersions.toSeq)
      burnTx = burn(version, master, ENOUGH_AMT, fee, ts + 10000)
    } yield (genesis, burnTx)

    forAll(setup) {
      case (genesis, registerTx) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(registerTx)), fs) { blockDiffEi =>
          blockDiffEi should produce("negative lto balance")
        }
    }
  }
}
