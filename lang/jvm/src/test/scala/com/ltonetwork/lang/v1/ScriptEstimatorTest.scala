package com.ltonetwork.lang.v1

import cats.data.EitherT
import cats.kernel.Monoid
import com.ltonetwork.lang.Common._
import com.ltonetwork.lang._
import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.compiler.Terms._
import com.ltonetwork.lang.v1.evaluator.FunctionIds._
import com.ltonetwork.lang.v1.evaluator.ctx._
import com.ltonetwork.lang.v1.evaluator.ctx.impl.PureContext
import com.ltonetwork.lang.v1.evaluator.ctx.impl.lto.Types.transferTransactionType
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.lang.v1.testing.ScriptGen
import monix.eval.Coeval
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ScriptEstimatorTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with ScriptGen with NoShrink {
  val Plus  = FunctionHeader.Native(SUM_LONG)
  val Minus = FunctionHeader.Native(SUB_LONG)
  val Gt    = FunctionHeader.Native(GT_LONG)

  val FunctionCosts: Map[FunctionHeader, Coeval[Long]] = Map[FunctionHeader, Long](Plus -> 100, Minus -> 10, Gt -> 10).mapValues(Coeval.now)

  private val ctx = {
    val tx = CaseObj(transferTransactionType.typeRef, Map("amount" -> 100000000L))
    Monoid
      .combine(PureContext.ctx,
               CTX(
                 Seq(transferTransactionType),
                 Map(("tx", (transferTransactionType.typeRef, LazyVal(EitherT.pure(tx))))),
                 Seq.empty
               ))
  }

  private def compile(code: String): EXPR = {
    val untyped = Parser(code).get.value
    CompilerV1(ctx.compilerContext, untyped).map(_._1).explicitGet()
  }

  private def estimate(functionCosts: collection.Map[FunctionHeader, Coeval[Long]], script: EXPR) =
    ScriptEstimator(ctx.evaluationContext.letDefs.keySet, functionCosts, script)

  property("successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(Plus, List(CONST_LONG(1), acc))
    }
    estimate(FunctionCosts, expr) shouldBe 'right
  }

  property("handles const expression correctly") {
    estimate(Map.empty, compile("false")).explicitGet() shouldBe 1
  }

  property("handles getter expression correctly") {
    estimate(Map.empty, compile("tx.amount")).explicitGet() shouldBe 2 + 2
  }

  property("evaluates let statement lazily") {
    val eager = "let t = 1+1; t"
    estimate(FunctionCosts, compile(eager)).explicitGet() shouldBe 5 + 102 + 2

    val lzy = "let t = 1+1; 2" // `t` is unused
    estimate(FunctionCosts, compile(lzy)).explicitGet() shouldBe 5 + 1

    val onceOnly = "let x = 2+2; let y = x-x; x-y" // evaluated once only
    estimate(FunctionCosts, compile(onceOnly)).explicitGet() shouldBe (5 + 102) + (5 + 14) + 14
  }

  property("ignores unused let statements") {
    val script = "let a = 1+2; let b = 2; let c = a+b; b" // `a` and `c` are unused
    estimate(FunctionCosts, compile(script)).explicitGet() shouldBe 5 + (5 + 1) + 5 + 2
  }

  property("recursive let statement") {
    // let v = v; v
    val expr = BLOCK(LET("v", REF("v")), REF("v"))
    estimate(Map.empty, expr) shouldBe 'right
  }

  property("evaluates if statement lazily") {
    val script = "let a = 1+2; let b = 3+4; let c = if (tx.amount > 5) then a else b; c"
    estimate(FunctionCosts, compile(script)).explicitGet() shouldBe (5 + 102) + 5 + (5 + 16 + 2) + 2
  }
}
