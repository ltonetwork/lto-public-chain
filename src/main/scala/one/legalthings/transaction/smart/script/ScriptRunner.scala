package one.legalthings.transaction.smart.script

import cats.implicits._
import one.legalthings.lang.v1.evaluator.EvaluatorV1
import one.legalthings.lang.v1.evaluator.ctx.EvaluationContext
import one.legalthings.lang.ExecutionError
import one.legalthings.state._
import monix.eval.Coeval
import one.legalthings.account.AddressScheme
import one.legalthings.transaction.Transaction
import one.legalthings.transaction.smart.BlockchainContext

object ScriptRunner {

  def apply[A, T <: Transaction](height: Int, tx: T, blockchain: Blockchain, script: Script): (EvaluationContext, Either[ExecutionError, A]) =
    script match {
      case Script.Expr(expr) =>
        val ctx = BlockchainContext.build(
          AddressScheme.current.chainId,
          Coeval.evalOnce(tx),
          Coeval.evalOnce(height),
          blockchain
        )
        EvaluatorV1[A](ctx, expr)

      case _ => (EvaluationContext.empty, "Unsupported script version".asLeft[A])
    }

}
