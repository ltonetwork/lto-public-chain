package one.legalthings.state

import java.util.concurrent.TimeUnit

import one.legalthings.lang.v1.compiler.CompilerV1
import one.legalthings.lang.v1.parser.Parser
import one.legalthings.settings.FunctionalitySettings
import one.legalthings.state.StateSyntheticBenchmark._
import one.legalthings.utils.dummyCompilerContext
import org.openjdk.jmh.annotations._
import org.scalacheck.Gen
import one.legalthings.account.PrivateKeyAccount
import one.legalthings.transaction.Transaction
import one.legalthings.transaction.smart.SetScriptTransaction
import one.legalthings.transaction.smart.script.v1.ScriptV1
import one.legalthings.transaction.transfer._

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class StateSyntheticBenchmark {

  @Benchmark
  def appendBlock_test(db: St): Unit = db.genAndApplyNextBlock()

  @Benchmark
  def appendBlock_smart_test(db: SmartSt): Unit = db.genAndApplyNextBlock()

}

object StateSyntheticBenchmark {

  @State(Scope.Benchmark)
  class St extends BaseState {
    protected override def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction] =
      for {
        amount    <- Gen.choose(1, waves(1))
        recipient <- accountGen
      } yield TransferTransactionV1.selfSigned(sender, recipient, amount, ts, 100000, Array.emptyByteArray).explicitGet()
  }

  @State(Scope.Benchmark)
  class SmartSt extends BaseState {

    override protected def updateFunctionalitySettings(base: FunctionalitySettings): FunctionalitySettings = {
      base.copy(preActivatedFeatures = Map(4.toShort -> 0))
    }

    protected override def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction] =
      for {
        recipient: PrivateKeyAccount <- accountGen
        amount                       <- Gen.choose(1, waves(1))
      } yield
        TransferTransactionV2
          .selfSigned(
            TransferTransactionV2.supportedVersions.head,
            sender,
            recipient.toAddress,
            amount,
            ts,
            1000000,
            Array.emptyByteArray
          )
          .explicitGet()

    @Setup
    override def init(): Unit = {
      super.init()

      val textScript    = "sigVerify(tx.bodyBytes,tx.proofs[0],tx.senderPk)"
      val untypedScript = Parser(textScript).get.value
      assert(untypedScript.size == 1)
      val typedScript = CompilerV1(dummyCompilerContext, untypedScript.head).explicitGet()._1

      val setScriptBlock = nextBlock(
        Seq(
          SetScriptTransaction
            .selfSigned(
              SetScriptTransaction.supportedVersions.head,
              richAccount,
              Some(ScriptV1(typedScript).explicitGet()),
              1000000,
              System.currentTimeMillis()
            )
            .explicitGet()
        )
      )

      applyBlock(setScriptBlock)
    }
  }

}
