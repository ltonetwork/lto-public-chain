package com.ltonetwork.lang.v1

import java.util.concurrent.TimeUnit

import com.ltonetwork.lang.v1.ScriptEstimatorBenchmark.St
import com.ltonetwork.utils
import monix.eval.Coeval
import org.junit.Ignore
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ScriptEstimatorBenchmark {
  @Benchmark
  @Ignore
  def apply_test(st: St, bh: Blackhole): Unit = bh.consume(ScriptEstimator(Set.empty, st.functionCosts, st.expr)) // fixing compilation
}

object ScriptEstimatorBenchmark {

  class St extends BigSum {
    val functionCosts: Map[FunctionHeader, Coeval[Long]] = utils.functionCosts
  }

}
