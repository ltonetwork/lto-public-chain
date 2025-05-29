package com.ltonetwork.features

case class BlockchainFeature private (id: Short, description: String)

object BlockchainFeatures {

  val UnknownFeature = "Unknown feature"

  val MinimalGeneratingBalance: BlockchainFeature = BlockchainFeature(1, "Minimum Generating Balance of 1000 LTO")
  val SmartAccounts: BlockchainFeature            = BlockchainFeature(4, "Smart Accounts")
  val AssociationTransaction: BlockchainFeature   = BlockchainFeature(10, "Association Transaction")
  val SponsorshipTransaction: BlockchainFeature   = BlockchainFeature(11, "Sponsorship Transaction")
  val BurnFeeture: BlockchainFeature              = BlockchainFeature(12, "Partial Fee Burn")
  val Cobalt: BlockchainFeature                   = BlockchainFeature(13, "Cobalt")
  val CobaltAlloy: BlockchainFeature              = BlockchainFeature(14, "Cobalt Alloy")
  val Juicy: BlockchainFeature                    = BlockchainFeature(15, "Juicy (Tokenomics)")
  val Titanium: BlockchainFeature                 = BlockchainFeature(16, "Titanium")
  val Palladium: BlockchainFeature                = BlockchainFeature(17, "Palladium")

  private val dict = Seq(
    SmartAccounts,
    AssociationTransaction,
    SponsorshipTransaction,
    BurnFeeture,
    Cobalt,
    CobaltAlloy,
    Juicy,
    Titanium,
    Palladium
  ).map(f => f.id -> f).toMap

  val preActivated: Set[Short] = Set(1, 2, 3, 5, 8).map(_.toShort) // consensus logic hardcoded
  val implemented: Set[Short]  = preActivated ++ dict.keySet

  def feature(id: Short): Option[BlockchainFeature] = dict.get(id)
  def featureOrUnknown(id: Short): BlockchainFeature = feature(id).getOrElse(BlockchainFeature(id, UnknownFeature))
}
