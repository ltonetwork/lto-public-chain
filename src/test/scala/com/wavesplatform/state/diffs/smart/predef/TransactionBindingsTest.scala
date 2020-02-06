package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.state._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.transaction.{ProvenTransaction, VersionedTransaction}
import play.api.libs.json.Json // For string escapes.

class TransactionBindingsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  def provenPart(t: ProvenTransaction): String = {
    def pg(i: Int) = s"let proof$i = t.proofs[$i] == base58'${t.proofs.proofs.applyOrElse(i, (_: Int) => ByteStr.empty).base58}'"
    s"""
       |   let id = t.id == base58'${t.id().base58}'
       |   let fee = t.fee == ${t.fee}
       |   let timestamp = t.timestamp == ${t.timestamp}
       |   let bodyBytes = t.bodyBytes == base64'${ByteStr(t.bodyBytes.apply()).base64}'
       |   let sender = t.sender == addressFromPublicKey(base58'${ByteStr(t.sender.publicKey).base58}')
       |   let senderPublicKey = t.senderPublicKey == base58'${ByteStr(t.sender.publicKey).base58}'
       |   let version = t.version == ${t match {
         case v: VersionedTransaction => v.version
         case _                       => 1
       }}
       |   ${Range(0, 8).map(pg).mkString("\n")}
     """.stripMargin
  }

  val assertProvenPart =
    "id && fee && timestamp && sender && senderPublicKey && proof0 && proof1 && proof2 && proof3 && proof4 && proof5 && proof6 && proof7 && bodyBytes && version"

  property("TransferTransaction binding") {
    forAll(Gen.oneOf(transferV1Gen, transferV2Gen)) { t =>
      // `version`  is not properly bound yet

      val result = runScript[Boolean](
        s"""
           |match tx {
           | case t : TransferTransaction  =>
           |   ${provenPart(t)}
           |   let amount = t.amount == ${t.amount}
           |   let recipient = t.recipient.bytes == base58'${t.recipient.cast[Address].map(_.bytes.base58).getOrElse("")}'
           |    let attachment = t.attachment == base58'${ByteStr(t.attachment).base58}'
           |   $assertProvenPart && amount && recipient && attachment
           | case other => throw()
           | }
           |""".stripMargin,
        t,
        'T'
      )
      result shouldBe Right(true)
    }
  }

  property("LeaseTransaction binding") {
    forAll(leaseGen) { t =>
      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : LeaseTransaction =>
          |   ${provenPart(t)}
          |   let amount = t.amount == ${t.amount}
          |   let recipient = t.recipient.bytes == base58'${t.recipient.cast[Address].map(_.bytes.base58).getOrElse("")}'
          |   $assertProvenPart && amount && recipient
          | case other => throw()
          | }
          |""".stripMargin,
        t,
        'T'
      )
      result shouldBe Right(true)
    }
  }

  property("LeaseCancelTransaction binding") {
    forAll(leaseCancelGen) { t =>
      val result = runScript[Boolean](
        s"""
          |match tx {
          | case t : LeaseCancelTransaction =>
          |   ${provenPart(t)}
          |   let leaseId = t.leaseId == base58'${t.leaseId.base58}'
          |   $assertProvenPart && leaseId
          | case other => throw()
          | }
          |""".stripMargin,
        t,
        'T'
      )
      result shouldBe Right(true)
    }
  }

  property("SetScriptTransaction binding") {
    forAll(setScriptTransactionGen) { t =>
      val result = runScript[Boolean](
        s"""
           |match tx {
           | case t : SetScriptTransaction =>
           |   ${provenPart(t)}
           |   let script = if (${t.script.isDefined}) then extract(t.script) == base64'${t.script
             .map(_.bytes().base64)
             .getOrElse("")}' else isDefined(t.script) == false
           |   $assertProvenPart && script
           | case other => throw()
           | }
           |""".stripMargin,
        t,
        'T'
      )
      result shouldBe Right(true)
    }
  }
  property("MassTransferTransaction binding") {
    forAll(massTransferGen) { t =>
      def pg(i: Int) =
        s"""let recipient$i =
           | t.transfers[$i].recipient.bytes == base58'${t.transfers(i).address.cast[Address].map(_.bytes.base58).getOrElse("")}'
           |let amount$i = t.transfers[$i].amount == ${t.transfers(i).amount}
         """.stripMargin

      val resString =
        if (t.transfers.isEmpty) assertProvenPart
        else
          assertProvenPart + s" &&" + {
            Range(0, t.transfers.length)
              .map(i => s"recipient$i && amount$i")
              .mkString(" && ")
          }

      val script = s"""
                      |match tx {
                      | case t : MassTransferTransaction =>
                      |     let transferCount = t.transferCount == ${t.transfers.length}
                      |     let totalAmount = t.totalAmount == ${t.transfers.map(_.amount).sum}
                      |     let attachment = t.attachment == base58'${ByteStr(t.attachment).base58}'
                      |     ${Range(0, t.transfers.length).map(pg).mkString("\n")}
                      |   ${provenPart(t)}
                      |   $resString && transferCount && totalAmount && attachment
                      | case other => throw()
                      | }
                      |""".stripMargin

      val result = runScript[Boolean](
        script,
        t,
        'T'
      )
      result shouldBe Right(true)
    }
  }

}
