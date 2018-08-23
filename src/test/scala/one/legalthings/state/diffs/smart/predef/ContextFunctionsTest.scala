package one.legalthings.state.diffs.smart.predef

import one.legalthings.lang.Global.MaxBase58Bytes
import one.legalthings.lang.v1.compiler.CompilerV1
import one.legalthings.lang.v1.parser.Parser
import one.legalthings.state._
import one.legalthings.state.diffs.smart.smartEnabledFS
import one.legalthings.state.diffs.{ENOUGH_AMT, assertDiffAndState}
import one.legalthings.utils.dummyCompilerContext
import one.legalthings.TransactionGen
import one.legalthings.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import one.legalthings.transaction.smart.SetScriptTransaction
import one.legalthings.transaction.smart.script.v1.ScriptV1
import one.legalthings.transaction.GenesisTransaction

class ContextFunctionsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val preconditionsAndPayments = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis1 = GenesisTransaction.create(master, ENOUGH_AMT * 3, ts).explicitGet()
    genesis2 = GenesisTransaction.create(recipient, ENOUGH_AMT * 3, ts).explicitGet()
    long            <- longEntryGen(dataAsciiKeyGen)
    bool            <- booleanEntryGen(dataAsciiKeyGen).filter(_.key != long.key)
    bin             <- binaryEntryGen(MaxBase58Bytes, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key)
    str             <- stringEntryGen(500, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key && e.key != bin.key)
    dataTransaction <- dataTransactionGenP(recipient, List(long, bool, bin, str))
    transfer        <- transferGeneratorP(ts, master, recipient.toAddress, 100000000L)

    untypedScript = {
      val r = Parser(scriptWithAllFunctions(dataTransaction, transfer)).get.value
      assert(r.size == 1)
      r.head
    }

    typedScript = {
      val compilerScript = CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1
      ScriptV1(compilerScript).explicitGet()
    }
    setScriptTransaction: SetScriptTransaction = SetScriptTransaction.selfSigned(1, recipient, Some(typedScript), 100000000L, ts).explicitGet()

  } yield (Seq(genesis1, genesis2), setScriptTransaction, dataTransaction, transfer)

  ignore("validation of all functions from contexts") {
    forAll(preconditionsAndPayments) {
      case ((genesis, setScriptTransaction, dataTransaction, transfer)) =>
        assertDiffAndState(smartEnabledFS) { append =>
          append(genesis).explicitGet()
          append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
          append(Seq(transfer)).explicitGet()
        }
    }
  }

}
