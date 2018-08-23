package one.legalthings.transaction.smart

import cats.kernel.Monoid
import one.legalthings.lang.v1.evaluator.ctx.EvaluationContext
import one.legalthings.lang.v1.evaluator.ctx.impl.waves.WavesContext
import one.legalthings.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import one.legalthings.state._
import monix.eval.Coeval
import one.legalthings.lang.Global
import one.legalthings.transaction._

object BlockchainContext {

  private val baseContext = Monoid.combine(PureContext.ctx, CryptoContext.build(Global)).evaluationContext

  def build(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain): EvaluationContext =
    Monoid.combine(baseContext, WavesContext.build(new WavesEnvironment(nByte, tx, h, blockchain)).evaluationContext)
}
