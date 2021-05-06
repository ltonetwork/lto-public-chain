package com.ltonetwork.state.diffs.smart.scenarios

import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.state.diffs.smart._
import com.ltonetwork.state._
import com.ltonetwork.state.diffs.{assertDiffAndState, assertDiffEi, produce}
import com.ltonetwork.utils.dummyCompilerContext
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.ltonetwork.lagonaki.mocks.TestBlock
import com.ltonetwork.transaction.GenesisTransaction
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.transfer._

class TransactionFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private def preconditionsTransferAndLease(
      code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV2)] = {
    val untyped = Parser(code).get.value
    val typed   = CompilerV1(dummyCompilerContext, untyped).explicitGet()._1
    preconditionsTransferAndLease(typed)
  }

  private val script =
    """
      |
      | match tx {
      | case ttx: TransferTransaction => tx.fee > 0
      | case other => false
      | }
    """.stripMargin

  property("accessing field of transaction without checking its type first results on exception") {
    forAll(preconditionsTransferAndLease(script)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }
}