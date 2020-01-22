package com.wavesplatform.features

case class BlockchainFeature private (id: Short, description: String)

object BlockchainFeatures {

  val DUMMY_FOR_TESTS_SmallerMinimalGeneratingBalance = BlockchainFeature(1, "Minimum Generating Balance of 1000 LTO")
  val SmartAccounts                   = BlockchainFeature(4, "Smart Accounts")
  val AssociationTransaction          = BlockchainFeature(10, "Association Transaction")
  val SponsorshipTransaction          = BlockchainFeature(11, "Sponsorship Transaction")

  private val dict = Seq(
    SmartAccounts,
    AssociationTransaction,
    SponsorshipTransaction
  ).map(f => f.id -> f).toMap

  val implemented: Set[Short] = dict.keySet

  def feature(id: Short): Option[BlockchainFeature] = dict.get(id)
}
