package com.ltonetwork.transaction.smart.script

import cats.implicits._
import com.ltonetwork.lang.v1.evaluator.EvaluatorV1
import com.ltonetwork.lang.v1.evaluator.ctx.EvaluationContext
import com.ltonetwork.lang.{ExecutionError, ExprEvaluator}
import com.ltonetwork.state._
import monix.eval.Coeval
import com.ltonetwork.account.AddressScheme
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.transaction.smart.BlockchainContext

object ScriptRunner {

  def apply[A, T <: Transaction](height: Int, tx: T, blockchain: Blockchain, script: Script): (ExprEvaluator.Log, Either[ExecutionError, A]) =
    script match {
      case Script.Expr(expr) =>
        val ctx = BlockchainContext.build(
          AddressScheme.current.chainId,
          Coeval.evalOnce(tx),
          Coeval.evalOnce(height),
          blockchain
        )
        EvaluatorV1.applywithLogging[A](ctx, expr)

      case _ => (List.empty, "Unsupported script version".asLeft[A])
    }

}
