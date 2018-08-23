package one.legalthings.state.diffs.smart.scenarios

import one.legalthings.lang.v1.compiler.CompilerV1
import one.legalthings.lang.v1.parser.Parser
import one.legalthings.state._
import one.legalthings.state.diffs._
import one.legalthings.state.diffs.smart._
import one.legalthings.utils.dummyCompilerContext
import one.legalthings.TransactionGen
import one.legalthings.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import one.legalthings.lagonaki.mocks.TestBlock

class OnlyTransferIsAllowedTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  ignore("transfer is allowed but lease is not due to predicate") {

    val scriptText =
      s"""
         |
         | match tx {
         |  case ttx: TransferTransaction | MassTransferTransaction =>
         |     sigVerify(ttx.bodyBytes,ttx.proofs[0],ttx.senderPublicKey)
         |  case other =>
         |     false
         | }
      """.stripMargin
    val untyped = Parser(scriptText).get.value
    assert(untyped.size == 1)
    val transferAllowed = CompilerV1(dummyCompilerContext, untyped.head).explicitGet()._1

    forAll(preconditionsTransferAndLease(transferAllowed)) {
      case (genesis, script, lease, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

}
