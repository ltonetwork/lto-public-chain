package one.legalthings.state.lang.v1

import java.util.concurrent.TimeUnit

import one.legalthings.lang.v1.evaluator.EvaluatorV1
import one.legalthings.lang.v1.evaluator.ctx.impl.PureContext
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 20)
@Measurement(iterations = 10)
class ScriptEvaluatorBenchmark {
  @Benchmark
  def apply_test(st: St, bh: Blackhole): Unit = bh.consume(EvaluatorV1[Boolean](st.context, st.expr))
}

object ScriptEvaluatorBenchmark {
  class St extends BigSum {
    val context = PureContext.evalContext
  }
}
