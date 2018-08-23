package one.legalthings.state.diffs.smart.performance

import one.legalthings.lang.v1.compiler.Terms._
import one.legalthings.lang.v1.compiler.CompilerV1
import one.legalthings.lang.v1.parser.Parser
import one.legalthings.metrics.Instrumented
import one.legalthings.state._
import one.legalthings.utils._
import one.legalthings.state.diffs._
import one.legalthings.state.diffs.smart._
import one.legalthings.{TransactionGen, WithDB}
import one.legalthings.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import one.legalthings.account.{PrivateKeyAccount, PublicKeyAccount}
import one.legalthings.lagonaki.mocks.TestBlock
import one.legalthings.transaction.GenesisTransaction
import one.legalthings.transaction.smart.script.v1.ScriptV1
import one.legalthings.transaction.transfer._

class SigVerifyPerformanceTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val AmtOfTxs = 10000

  private def simpleSendGen(from: PrivateKeyAccount, to: PublicKeyAccount, ts: Long): Gen[TransferTransactionV1] =
    for {
      amt <- smallFeeGen
      fee <- smallFeeGen
    } yield TransferTransactionV1.selfSigned(from, to.toAddress, amt, ts, fee, Array.emptyByteArray).explicitGet()

  private def scriptedSendGen(from: PrivateKeyAccount, to: PublicKeyAccount, ts: Long): Gen[TransferTransactionV2] =
    for {
      version <- Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)
      amt     <- smallFeeGen
      fee     <- smallFeeGen
    } yield TransferTransactionV2.selfSigned(version, from, to.toAddress, amt, ts, fee, Array.emptyByteArray).explicitGet()

  private def differentTransfers(typed: EXPR) =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      amt       <- smallFeeGen
      fee       <- smallFeeGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(typed).explicitGet())
      transfer       = simpleSendGen(master, recipient, ts)
      scriptTransfer = scriptedSendGen(master, recipient, ts)
      transfers       <- Gen.listOfN(AmtOfTxs, transfer)
      scriptTransfers <- Gen.listOfN(AmtOfTxs, scriptTransfer)
    } yield (genesis, setScript, transfers, scriptTransfers)

  ignore("parallel native signature verification vs sequential scripted signature verification") {
    val textScript    = "sigVerify(tx.bodyBytes,tx.proofs[0],tx.senderPk)"
    val untypedScript = Parser(textScript).get.value
    assert(untypedScript.size == 1)
    val typedScript = CompilerV1(dummyCompilerContext, untypedScript.head).explicitGet()._1

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
