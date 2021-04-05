package com.ltonetwork.transaction.smart

import cats.kernel.Monoid
import com.ltonetwork.lang.Global
import com.ltonetwork.lang.v1.evaluator.ctx.EvaluationContext
import com.ltonetwork.lang.v1.evaluator.ctx.impl.lto.LtoContext
import com.ltonetwork.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.ltonetwork.state._
import monix.eval.Coeval
import com.ltonetwork.transaction._

object BlockchainContext {

  private val baseContext = Monoid.combine(PureContext.ctx, CryptoContext.build(Global)).evaluationContext

  def build(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain): EvaluationContext =
    Monoid.combine(baseContext, LtoContext.build(new LtoEnvironment(nByte, tx, h, blockchain)).evaluationContext)
}
