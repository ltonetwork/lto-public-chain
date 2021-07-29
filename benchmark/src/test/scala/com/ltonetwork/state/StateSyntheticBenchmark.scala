package com.ltonetwork.state

import java.util.concurrent.TimeUnit

import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state.StateSyntheticBenchmark._
import com.ltonetwork.utils.dummyCompilerContext
import org.openjdk.jmh.annotations._
import org.scalacheck.Gen
import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.transfer._

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
        amount    <- Gen.choose(1, lto(1))
        recipient <- accountGen
      } yield TransferTransaction.signed(1, ts, sender, 100000, recipient, amount, Array.emptyByteArray).explicitGet()
  }

  @State(Scope.Benchmark)
  class SmartSt extends BaseState {

    override protected def updateFunctionalitySettings(base: FunctionalitySettings): FunctionalitySettings = {
      base.copy(preActivatedFeatures = Map(4.toShort -> 0))
    }

    protected override def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction] =
      for {
        recipient: PrivateKeyAccount <- accountGen
        amount                       <- Gen.choose(1, lto(1))
      } yield
        TransferTransaction
          .signed(
            2,
            ts,
            sender,
            1000000,
            recipient.toAddress,
            amount,
            Array.emptyByteArray
          )
          .explicitGet()

    @Setup
    override def init(): Unit = {
      super.init()

      val textScript    = "sigVerify(tx.bodyBytes,tx.proofs[0],tx.senderPk)"
      val untypedScript = Parser(textScript).get.value
      val typedScript   = CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1

      val setScriptBlock = nextBlock(
        Seq(
          SetScriptTransaction
            .signed(
              1,
              System.currentTimeMillis(),
              richAccount,
              1000000,
              Some(ScriptV1(typedScript).explicitGet())
            )
            .explicitGet()
        )
      )

      applyBlock(setScriptBlock)
    }
  }

}
