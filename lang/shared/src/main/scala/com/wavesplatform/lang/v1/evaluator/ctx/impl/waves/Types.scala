package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.CaseType

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

  val createAliasTransactionType = CaseType(
    "CreateAliasTransaction",
    List(
      "alias" -> STRING,
    ) ++ header ++ proven
  )

  val paymentTransactionType = CaseType(
    "PaymentTransaction",
    List(
      "amount"    -> LONG,
      "recipient" -> addressType.typeRef,
    ) ++ header ++ proven
  )

  val sponsorFeeTransactionType = CaseType(
    "SponsorFeeTransaction",
    List(
      "assetId"              -> BYTEVECTOR,
      "minSponsoredAssetFee" -> optionLong
    ) ++ header ++ proven
  )

  val buyType  = CaseType("Buy", List.empty)
  val sellType = CaseType("Sell", List.empty)

  val ordTypeType = UNION(buyType.typeRef, sellType.typeRef)

  val assetPairType = CaseType("AssetPair", List("amountAsset" -> optionByteVector, "priceAsset" -> optionByteVector))
  val orderType = CaseType(
    "Order",
    List(
      "id"               -> BYTEVECTOR,
      "matcherPublicKey" -> BYTEVECTOR,
      "assetPair"        -> assetPairType.typeRef,
      "orderType"        -> ordTypeType,
      "price"            -> LONG,
      "amount"           -> LONG,
      "timestamp"        -> LONG,
      "expiration"       -> LONG,
      "matcherFee"       -> LONG,
    ) ++ proven
  )
  val exchangeTransactionType = CaseType(
    "ExchangeTransaction",
    List("buyOrder"       -> orderType.typeRef,
         "sellOrder"      -> orderType.typeRef,
         "price"          -> LONG,
         "amount"         -> LONG,
         "buyMatcherFee"  -> LONG,
         "sellMatcherFee" -> LONG)
      ++ header ++ proven
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
  val obsoleteTransactionTypes = List(genesisTransactionType)

  val activeTransactionTypes = List(
    transferTransactionType,
    leaseTransactionType,
    leaseCancelTransactionType,
    massTransferTransactionType,
    setScriptTransactionType,
    anchorTransactionType
  )

  val transactionTypes = obsoleteTransactionTypes ++ activeTransactionTypes

  val outgoingTransactionType = UNION.create(activeTransactionTypes.map(_.typeRef))
  val anyTransactionType      = UNION.create(transactionTypes.map(_.typeRef))

  val wavesTypes = Seq(addressType, transfer) ++ transactionTypes
}
