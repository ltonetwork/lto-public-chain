package com.ltonetwork.lang.v1.evaluator.ctx

import cats.data.EitherT
import cats.implicits._
import com.ltonetwork.lang.ExprEvaluator.LogCallback
import com.ltonetwork.lang.TrampolinedExecResult
import monix.eval.Coeval

sealed trait LazyVal {
  val value: TrampolinedExecResult[Any]
}

object LazyVal {
  private case class LazyValImpl(v: TrampolinedExecResult[Any], lc: LogCallback) extends LazyVal {
    override val value: TrampolinedExecResult[Any] = EitherT(
        Coeval.evalOnce(
          v.value
            .flatTap(a => Coeval.evalOnce(lc(a)))
            .apply()
        )
    )
  }

  def apply(v: TrampolinedExecResult[Any], lc: LogCallback = _ => ()): LazyVal = LazyValImpl(v, lc)
}
