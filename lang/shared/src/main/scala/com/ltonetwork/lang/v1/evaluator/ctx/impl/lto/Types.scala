package com.ltonetwork.lang.v1.evaluator.ctx.impl.lto

import com.ltonetwork.lang.v1.compiler.Types._
import com.ltonetwork.lang.v1.evaluator.ctx.CaseType

// TODO Add missing types for: Association and Sponsorship
object Types {

  val addressType = CaseType("Address", List("bytes" -> BYTEVECTOR))

  val transfer = CaseType("Transfer", List("recipient" -> addressType.typeRef, "amount" -> LONG))

  val optionByteVector     = UNION(BYTEVECTOR, UNIT)
  val optionAddress        = UNION(addressType.typeRef, UNIT)
  val optionLong           = UNION(LONG, UNIT)
  val listByteVector: LIST = LIST(BYTEVECTOR)
  val listTransfers        = LIST(transfer.typeRef)

  val header = List(
    "id"        -> BYTEVECTOR,
    "fee"       -> LONG,
    "timestamp" -> LONG,
    "version"   -> LONG,
  )
  val proven = List(
    "sender"          -> addressType.typeRef,
    "senderPublicKey" -> BYTEVECTOR,
    "bodyBytes"       -> BYTEVECTOR,
    "proofs"          -> listByteVector
  )

  val genesisTransactionType = CaseType(
    "GenesisTransaction",
    List("amount" -> LONG, "recipient" -> addressType.typeRef) ++ header
  )

  val transferTransactionType = CaseType(
    "TransferTransaction",
    List(
      "amount"     -> LONG,
      "recipient"  -> addressType.typeRef,
      "attachment" -> BYTEVECTOR
    ) ++ header ++ proven
  )

  val leaseTransactionType = CaseType(
    "LeaseTransaction",
    List(
      "amount"    -> LONG,
      "recipient" -> addressType.typeRef,
    ) ++ header ++ proven
  )

  val leaseCancelTransactionType = CaseType(
    "LeaseCancelTransaction",
    List(
      "leaseId" -> BYTEVECTOR,
    ) ++ header ++ proven
  )

  private val dataEntryValueType = UNION(LONG, BOOLEAN, BYTEVECTOR, STRING)
  val dataEntryType              = CaseType("DataEntry", List("key" -> STRING, "value" -> dataEntryValueType))

  val dataTransactionType = CaseType(
    "DataTransaction",
    List("data" -> LIST(dataEntryType.typeRef)) ++ header ++ proven
  )

  val massTransferTransactionType = CaseType(
    "MassTransferTransaction",
    List(
      "totalAmount"   -> LONG,
      "transfers"     -> listTransfers,
      "transferCount" -> LONG,
      "attachment"    -> BYTEVECTOR
    ) ++ header ++ proven
  )

  val setScriptTransactionType = CaseType(
    "SetScriptTransaction",
    List(
      "script" -> optionByteVector
    ) ++ header ++ proven
  )
  val anchorTransactionType    = CaseType("AnchorTransaction", List() ++ header ++ proven)
  val registerTransactionType  = CaseType("RegisterTransaction", List() ++ header ++ proven)
  val obsoleteTransactionTypes = List(genesisTransactionType)

  val activeTransactionTypes = List(
    transferTransactionType,
    leaseTransactionType,
    leaseCancelTransactionType,
    massTransferTransactionType,
    setScriptTransactionType,
    anchorTransactionType,
    registerTransactionType
  )

  val transactionTypes = obsoleteTransactionTypes ++ activeTransactionTypes

  val outgoingTransactionType = UNION.create(activeTransactionTypes.map(_.typeRef))
  val anyTransactionType      = UNION.create(transactionTypes.map(_.typeRef))

  val ltoTypes = Seq(addressType, transfer) ++ transactionTypes
}
