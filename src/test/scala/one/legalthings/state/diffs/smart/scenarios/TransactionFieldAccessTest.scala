package one.legalthings.state.diffs.smart.scenarios

import one.legalthings.lang.v1.compiler.CompilerV1
import one.legalthings.lang.v1.parser.Parser
import one.legalthings.state.diffs.smart._
import one.legalthings.state._
import one.legalthings.state.diffs.{assertDiffAndState, assertDiffEi, produce}
import one.legalthings.utils.dummyCompilerContext
import one.legalthings.TransactionGen
import one.legalthings.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Ignore, Matchers, PropSpec}
import one.legalthings.lagonaki.mocks.TestBlock
import one.legalthings.transaction.GenesisTransaction
import one.legalthings.transaction.lease.LeaseTransaction
import one.legalthings.transaction.smart.SetScriptTransaction
import one.legalthings.transaction.transfer._

@Ignore
class TransactionFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private def preconditionsTransferAndLease(
      code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV1)] = {
    val untyped = Parser(code).get.value
    assert(untyped.size == 1)
    val typed = CompilerV1(dummyCompilerContext, untyped.head).explicitGet()._1
    preconditionsTransferAndLease(typed)
  }

  private val script =
    """
      |
      | match tx {
      | case ttx: TransferTransaction =>
      |       isDefined(ttx.assetId)==false
      |   case other =>
      |       false
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
