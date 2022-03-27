package com.ltonetwork.state.diffs.smart.performance

import com.ltonetwork.lang.v1.compiler.Terms._
import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.metrics.Instrumented
import com.ltonetwork.state._
import com.ltonetwork.utils._
import com.ltonetwork.state.diffs._
import com.ltonetwork.state.diffs.smart._
import com.ltonetwork.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatestplus.scalacheck.Checkers
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.block.TestBlock
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.transfer._

class SigVerifyPerformanceTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val AmtOfTxs = 10000

  private def simpleSendGen(from: PrivateKeyAccount, to: PublicKeyAccount, ts: Long): Gen[TransferTransaction] =
    for {
      amt <- smallFeeGen
      fee <- smallFeeGen
    } yield TransferTransaction.signed(1, ts, from, fee, to.toAddress, amt, Array.emptyByteArray).explicitGet()

  private def scriptedSendGen(from: PrivateKeyAccount, to: PublicKeyAccount, ts: Long): Gen[TransferTransaction] =
    for {
      version <- Gen.oneOf(TransferTransaction.supportedVersions.toSeq)
      amt     <- smallFeeGen
      fee     <- smallFeeGen
    } yield TransferTransaction.signed(version, ts, from, fee, to.toAddress, amt, Array.emptyByteArray).explicitGet()

  private def differentTransfers(typed: EXPR) =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      amt       <- smallFeeGen
      fee       <- smallFeeGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(typed).explicitGet(), ts + 1)
      transfer       = simpleSendGen(master, recipient, ts)
      scriptTransfer = scriptedSendGen(master, recipient, ts)
      transfers       <- Gen.listOfN(AmtOfTxs, transfer)
      scriptTransfers <- Gen.listOfN(AmtOfTxs, scriptTransfer)
    } yield (genesis, setScript, transfers, scriptTransfers)

  ignore("parallel native signature verification vs sequential scripted signature verification") {
    val textScript    = "sigVerify(tx.bodyBytes,tx.proofs[0],tx.senderPk)"
    val untypedScript = Parser(textScript).get.value
    val typedScript   = CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1

    forAll(differentTransfers(typedScript)) {
      case (gen, setScript, transfers, scriptTransfers) =>
        def simpleCheck(): Unit = assertDiffAndState(Seq(TestBlock.create(Seq(gen))), TestBlock.create(transfers), smartEnabledFS) { case _ => }
        def scriptedCheck(): Unit =
          assertDiffAndState(Seq(TestBlock.create(Seq(gen, setScript))), TestBlock.create(scriptTransfers), smartEnabledFS) {
            case _ =>
          }

        val simeplCheckTime   = Instrumented.withTime(simpleCheck())._2
        val scriptedCheckTime = Instrumented.withTime(scriptedCheck())._2
        println(s"[parallel] simple check time: $simeplCheckTime ms,\t [seqential] scripted check time: $scriptedCheckTime ms")
    }

  }
}
