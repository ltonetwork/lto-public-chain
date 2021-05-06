package com.ltonetwork.lang.v1.evaluator.ctx.impl.lto

import com.ltonetwork.lang.v1.evaluator.ctx.CaseObj
import com.ltonetwork.lang.v1.evaluator.ctx.impl.PureContext.fromOption
import com.ltonetwork.lang.v1.traits.Tx._
import com.ltonetwork.lang.v1.traits._
import scodec.bits.ByteVector

object Bindings {
  import Types._

  private def headerPart(tx: Header): Map[String, Any] = Map(
    "id"        -> tx.id,
    "fee"       -> tx.fee,
    "timestamp" -> tx.timestamp,
    "version"   -> tx.version,
  )

  private def proofsPart(existingProofs: IndexedSeq[ByteVector]) =
    "proofs" -> (existingProofs ++ Seq.fill(8 - existingProofs.size)(ByteVector.empty)).toIndexedSeq

  private def provenTxPart(tx: Proven): Map[String, Any] =
    Map(
      "sender"          -> senderObject(tx.sender),
      "senderPublicKey" -> tx.senderPk,
      "bodyBytes"       -> tx.bodyBytes,
      proofsPart(tx.proofs)
    ) ++ headerPart(tx.h)

  private def mapRecipient(r: LangAddress) = "recipient" -> senderObject(r)

  def senderObject(sender: LangAddress): CaseObj = CaseObj(addressType.typeRef, Map("bytes" -> sender.bytes))

  def transactionObject(tx: Tx): CaseObj =
    tx match {
      case Tx.Genesis(h, amount, recipient) =>
        CaseObj(genesisTransactionType.typeRef, Map("amount" -> amount) ++ headerPart(h) + mapRecipient(recipient))
      case Tx.Transfer(p, amount, recipient, attachment) =>
        CaseObj(
          transferTransactionType.typeRef,
          Map(
            "amount"     -> amount,
            "attachment" -> attachment
          ) ++ provenTxPart(p) + mapRecipient(recipient)
        )
      case Lease(p, amount, recipient) =>
        CaseObj(
          leaseTransactionType.typeRef,
          Map(
            "amount" -> amount,
          ) ++ provenTxPart(p) + mapRecipient(recipient)
        )
      case LeaseCancel(p, leaseId) =>
        CaseObj(
          leaseCancelTransactionType.typeRef,
          Map(
            "leaseId" -> leaseId,
          ) ++ provenTxPart(p)
        )

      case MassTransfer(p, transferCount, totalAmount, transfers, attachment) =>
        CaseObj(
          massTransferTransactionType.typeRef,
          Map(
            "transfers" -> transfers
              .map(bv => CaseObj(transfer.typeRef, Map(mapRecipient(bv.recipient), "amount" -> bv.amount))),
            "transferCount" -> transferCount,
            "totalAmount"   -> totalAmount,
            "attachment"    -> attachment
          ) ++ provenTxPart(p)
        )
      case SetScript(p, scriptOpt) =>
        CaseObj(setScriptTransactionType.typeRef, Map("script" -> fromOption(scriptOpt)) ++ provenTxPart(p))
    }

}
