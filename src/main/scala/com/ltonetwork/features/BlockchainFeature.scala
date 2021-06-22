package com.ltonetwork.features

case class BlockchainFeature private (id: Short, description: String)

object BlockchainFeatures {

  val UnknownFeature = "Unknown feature"

  val SmallerMinimalGeneratingBalance: BlockchainFeature = BlockchainFeature(1, "Minimum Generating Balance of 1000 LTO")
  val SmartAccounts: BlockchainFeature                   = BlockchainFeature(4, "Smart Accounts")
  val AssociationTransaction: BlockchainFeature          = BlockchainFeature(10, "Association Transaction")
  val SponsorshipTransaction: BlockchainFeature          = BlockchainFeature(11, "Sponsorship Transaction")
  val BurnFeeture: BlockchainFeature                     = BlockchainFeature(12, "Partial Fee Burn")
  val TransactionsV3: BlockchainFeature                  = BlockchainFeature(13, "Transactions v3")

  private val dict = Seq(
    SmartAccounts,
    AssociationTransaction,
    SponsorshipTransaction,
    BurnFeeture,
    //TransactionsV3,
  ).map(f => f.id -> f).toMap

  private val preActivated: Set[Short] = Set(1, 2, 3, 5, 8).map(_.toShort) // consensus logic hardcoded
  val implemented: Set[Short]          = preActivated ++ dict.keySet

  def feature(id: Short): Option[BlockchainFeature] = dict.get(id)
}
