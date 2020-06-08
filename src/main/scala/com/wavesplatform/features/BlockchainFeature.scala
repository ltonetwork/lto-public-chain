package com.wavesplatform.features

case class BlockchainFeature private (id: Short, description: String)

object BlockchainFeatures {

  val UnknownFeature = "Unknown feature"

  val DUMMY_FOR_TESTS_SmallerMinimalGeneratingBalance = BlockchainFeature(1, "Minimum Generating Balance of 1000 LTO")
  val SmartAccounts                                   = BlockchainFeature(4, "Smart Accounts")
  val AssociationTransaction                          = BlockchainFeature(10, "Association Transaction")
  val SponsorshipTransaction                          = BlockchainFeature(11, "Sponsorship Transaction")
  val BurnFeeture = BlockchainFeature(12, "Partial Fee Burn")


  private val dict = Seq(
    SmartAccounts,
    AssociationTransaction,
    SponsorshipTransaction,
    BurnFeeture
  ).map(f => f.id -> f).toMap

  private val preActivated: Set[Short] = Set(1, 2, 3, 5, 8).map(_.toShort) // consensus logic hardcoded
  val implemented: Set[Short]          = preActivated ++ dict.keySet

  def feature(id: Short): Option[BlockchainFeature] = dict.get(id)
}
