package com.ltonetwork.state.diffs.smart.scenarios

import com.ltonetwork.lang.Global
import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.evaluator.EvaluatorV1
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.state._
import com.ltonetwork.state.diffs._
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.utils._
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class NotaryControlledTransferScenartioTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink {
  val preconditions: Gen[(Seq[GenesisTransaction], DataTransaction, TransferTransaction, DataTransaction, DataTransaction, TransferTransaction)] =
    for {
      company  <- accountGen
      king     <- accountGen
      notary   <- accountGen
      accountA <- accountGen
      accountB <- accountGen
      ts       <- timestampGen
      genesis1 = GenesisTransaction.create(company, ENOUGH_AMT, ts).explicitGet()
      genesis2 = GenesisTransaction.create(king, ENOUGH_AMT, ts).explicitGet()
      genesis3 = GenesisTransaction.create(notary, ENOUGH_AMT, ts).explicitGet()
      genesis4 = GenesisTransaction.create(accountA, ENOUGH_AMT, ts).explicitGet()
      genesis5 = GenesisTransaction.create(accountB, ENOUGH_AMT, ts).explicitGet()

      assetScript = s"""
                    |
                    | match tx {
                    |   case ttx: TransferTransaction =>
                    |      let king = Address(base58'${king.address}')
                    |      let company = Address(base58'${company.address}')
                    |      let notary1 = addressFromPublicKey(extract(getBinary(king, "notary1PK")))
                    |      let txIdBase58String = toBase58String(ttx.id)
                    |      let isNotary1Agreed = match getBoolean(notary1,txIdBase58String) {
                    |        case b : Boolean => b
                    |        case _ : Unit => false
                    |      }
                    |      let recipientAddress = addressFromRecipient(ttx.recipient)
                    |      let recipientAgreement = getBoolean(recipientAddress,txIdBase58String)
                    |      let isRecipientAgreed = if(isDefined(recipientAgreement)) then extract(recipientAgreement) else false
                    |      let senderAddress = addressFromPublicKey(ttx.senderPublicKey)
                    |      senderAddress.bytes == company.bytes || (isNotary1Agreed && isRecipientAgreed)
                    |   case other => throw()
                    | }
        """.stripMargin

      untypedScript = {
        val r = Parser(assetScript).get.value
        r
      }

      typedScript = ScriptV1(CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1).explicitGet()

      kingDataTransaction = DataTransaction
        .signed(1, ts + 1, king, 1000, List(BinaryDataEntry("notary1PK", ByteStr(notary.publicKey))))
        .explicitGet()

      transferFromCompanyToA = TransferTransaction
        .signed(1, ts + 20, company, 1, accountA, 1000, Array.empty)
        .explicitGet()

      transferFromAToB = TransferTransaction
        .signed(1, ts + 30, accountA, 1, accountB, 1000, Array.empty)
        .explicitGet()

      notaryDataTransaction = DataTransaction
        .signed(1, ts + 4, notary, 1000, List(BooleanDataEntry(transferFromAToB.id().base58, true)))
        .explicitGet()

      accountBDataTransaction = DataTransaction
        .signed(1, ts + 5, accountB, 1000, List(BooleanDataEntry(transferFromAToB.id().base58, true)))
        .explicitGet()
    } yield
      (Seq(genesis1, genesis2, genesis3, genesis4, genesis5),
       kingDataTransaction,
       transferFromCompanyToA,
       notaryDataTransaction,
       accountBDataTransaction,
       transferFromAToB)

  private def eval[T](code: String) = {
    val untyped = Parser(code).get.value
    val typed   = CompilerV1(dummyCompilerContext, untyped).map(_._1)
    typed.flatMap(EvaluatorV1.applywithLogging[T](dummyEvaluationContext, _)._2)
  }

  property("Script toBase58String") {
    val s = "AXiXp5CmwVaq4Tp6h6"
    eval[Boolean](s"""toBase58String(base58'$s') == \"$s\"""").explicitGet() shouldBe true
  }

  property("Script toBase64String") {
    val s = "Kl0pIkOM3tRikA=="
    eval[Boolean](s"""toBase64String(base64'$s') == \"$s\"""").explicitGet() shouldBe true
  }

  property("addressFromString() returns None when address is too long") {
    import Global.MaxAddressLength
    val longAddress = "A" * (MaxAddressLength + 1)
    eval[Any](s"""addressFromString("$longAddress")""") shouldBe Right(())
  }

}
