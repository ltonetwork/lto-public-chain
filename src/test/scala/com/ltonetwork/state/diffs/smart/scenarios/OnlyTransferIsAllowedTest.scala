package com.ltonetwork.state.diffs.smart.scenarios

import com.ltonetwork.block.TestBlock
import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.state._
import com.ltonetwork.state.diffs._
import com.ltonetwork.state.diffs.smart._
import com.ltonetwork.utils.dummyCompilerContext
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class OnlyTransferIsAllowedTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink {

  property("transfer is allowed but lease is not due to predicate") {

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
    val untyped         = Parser(scriptText).get.value
    val transferAllowed = CompilerV1(dummyCompilerContext, untyped).explicitGet()._1

    forAll(preconditionsTransferAndLease(transferAllowed)) {
      case (genesis, script, lease, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

}
