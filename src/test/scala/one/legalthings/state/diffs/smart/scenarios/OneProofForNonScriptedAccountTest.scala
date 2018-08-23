package one.legalthings.state.diffs.smart.scenarios

import one.legalthings.lang.v1.compiler.Terms._
import one.legalthings.state._
import one.legalthings.state.diffs.smart.smartEnabledFS
import one.legalthings.state.diffs.{ENOUGH_AMT, assertDiffEi, produce}
import one.legalthings.TransactionGen
import one.legalthings.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import one.legalthings.lagonaki.mocks.TestBlock
import one.legalthings.transaction.smart.script.v1.ScriptV1
import one.legalthings.transaction.transfer._
import one.legalthings.transaction.{GenesisTransaction, Proofs}

class OneProofForNonScriptedAccountTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("exactly 1 proof required for non-scripted accounts") {
    val s = for {
      version   <- Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)
      master    <- accountGen
      recepient <- accountGen
      amt       <- positiveLongGen
      fee       <- smallFeeGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(TRUE).explicitGet())
      transfer = TransferTransactionV2.selfSigned(version, master, recepient, amt, ts, fee, Array.emptyByteArray).explicitGet()
    } yield (genesis, setScript, transfer)

    forAll(s) {
      case ((genesis, script, transfer)) =>
        val transferWithExtraProof = transfer.copy(proofs = Proofs(Seq(ByteStr.empty, ByteStr(Array(1: Byte)))))
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transferWithExtraProof)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("must have exactly 1 proof"))
    }
  }

}
