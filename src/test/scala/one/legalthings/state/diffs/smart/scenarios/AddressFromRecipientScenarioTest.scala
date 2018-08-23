package one.legalthings.state.diffs.smart.scenarios

import one.legalthings.lang.v1.compiler.CompilerV1
import one.legalthings.lang.v1.evaluator.EvaluatorV1
import one.legalthings.lang.v1.evaluator.ctx.CaseObj
import one.legalthings.lang.v1.parser.Parser
import one.legalthings.state._
import one.legalthings.state.diffs.{ENOUGH_AMT, assertDiffAndState, produce}
import one.legalthings.TransactionGen
import fastparse.core.Parsed
import monix.eval.Coeval
import one.legalthings.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import one.legalthings.account.{AddressOrAlias, AddressScheme, PrivateKeyAccount}
import one.legalthings.lagonaki.mocks.TestBlock
import one.legalthings.transaction.smart.BlockchainContext
import one.legalthings.transaction.transfer._
import one.legalthings.transaction.{CreateAliasTransaction, GenesisTransaction, Transaction}

class AddressFromRecipientScenarioTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndAliasCreations: Gen[(Seq[GenesisTransaction], CreateAliasTransaction, TransferTransactionV1, TransferTransactionV1)] = for {
    master                   <- accountGen
    ts                       <- timestampGen
    other: PrivateKeyAccount <- accountGen
    genesis1: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    genesis2: GenesisTransaction = GenesisTransaction.create(other, ENOUGH_AMT, ts).explicitGet()
    alias              <- aliasGen
    fee                <- smallFeeGen
    aliasTx            <- createAliasGen(other, alias, fee, ts)
    transferViaAddress <- transferGeneratorP(master, other, None, None)
    transferViaAlias   <- transferGeneratorP(master, AddressOrAlias.fromBytes(alias.bytes.arr, 0).explicitGet()._1, None, None)
  } yield (Seq(genesis1, genesis2), aliasTx, transferViaAddress, transferViaAlias)

  def evalScript(tx: Transaction, blockchain: Blockchain): Either[one.legalthings.lang.ExecutionError, CaseObj] = {
    val context =
      BlockchainContext.build(AddressScheme.current.chainId, Coeval.evalOnce(tx), Coeval.evalOnce(blockchain.height), blockchain)

    val Parsed.Success(expr, _) = Parser("""
        | match tx {
        |  case t : TransferTransaction =>  addressFromRecipient(t.recipient)
        |  case other => throw
        |  }
        |  """.stripMargin)
    assert(expr.size == 1)
    val Right((typedExpr, _)) = CompilerV1(one.legalthings.utils.dummyCompilerContext, expr.head)
    EvaluatorV1[CaseObj](context, typedExpr)._2
  }

  ignore("Script can resolve AddressOrAlias") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, aliasTx, transferViaAddress, transferViaAlias) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(aliasTx))) {
          case (_, state) =>
            val addressBytes = evalScript(transferViaAddress, state).explicitGet().fields("bytes").asInstanceOf[ByteVector]
            addressBytes.toArray.sameElements(transferViaAddress.recipient.bytes.arr) shouldBe true
            val resolvedAddressBytes = evalScript(transferViaAlias, state).explicitGet().fields("bytes").asInstanceOf[ByteVector]

            resolvedAddressBytes.toArray.sameElements(transferViaAddress.recipient.bytes.arr) shouldBe true
        }
    }
  }

  ignore("Script can't resolve alias that doesn't exist") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, _, _, transferViaAlias) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq())) {
          case (_, state) =>
            evalScript(transferViaAlias, state) should produce("AliasDoesNotExist")
        }
    }
  }
}
