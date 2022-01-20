package com.ltonetwork.state

import com.ltonetwork.account.Address
import com.ltonetwork.block.Block.BlockId
import com.ltonetwork.block.{Block, BlockHeader}
import com.ltonetwork.state.reader.LeaseDetails
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.transaction.association.AssociationTransaction
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.smart.script.Script

trait Blockchain {

  def carryFee: Long
  def height: Int
  def score: BigInt
  def scoreOf(blockId: ByteStr): Option[BigInt]

  def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)]
  def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)]

  def lastBlock: Option[Block]
  def blockBytes(height: Int): Option[Array[Byte]]
  def blockBytes(blockId: ByteStr): Option[Array[Byte]]

  def heightOf(blockId: ByteStr): Option[Int]

  /** Returns the most recent block IDs, starting from the most recent  one */
  def lastBlockIds(howMany: Int): Seq[ByteStr]

  /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
  def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]]

  def parent(block: Block, back: Int = 1): Option[Block]

  /** Features related */
  def approvedFeatures: Map[Short, Int]
  def activatedFeatures: Map[Short, Int]
  def featureVotes(height: Int): Map[Short, Int]

  def feePrice: Long
  def feePrice(height: Int): Long

  def portfolio(a: Address): Portfolio

  def transactionInfo(id: ByteStr): Option[(Int, Transaction)]
  def transactionHeight(id: ByteStr): Option[Int]

  def addressTransactions(address: Address, types: Set[Byte], count: Int, from: Int): Seq[(Int, Transaction)]

  def containsTransaction(id: ByteStr): Boolean
  def forgetTransactions(pred: (ByteStr, Long) => Boolean): Map[ByteStr, Long]
  def learnTransactions(values: Map[ByteStr, Long]): Unit

  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]

  /** Retrieves LTO balance snapshot in the [from, to] range (inclusive) */
  def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot]

  def accountScript(address: Address): Option[Script]
  def hasScript(address: Address): Boolean

  def accountData(acc: Address): AccountDataInfo
  def accountData(acc: Address, key: String): Option[DataEntry[_]]

  def balance(address: Address): Long

  def ltoDistribution(height: Int): Map[Address, Long]

  // the following methods are used exclusively by patches
  def allActiveLeases: Set[LeaseTransaction]

  def associations(address: Address): Blockchain.Associations

  def sponsorOf(address: Address): List[Address]

  /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
    *
    * @note Portfolios passed to `pf` only contain LTO and Leasing balances to improve performance */
  def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A]

  def append(diff: Diff, carryFee: Long, block: Block): Unit
  def rollbackTo(targetBlockId: ByteStr): Seq[Block]
}
object Blockchain {
  case class Associations(outgoing: List[(Int, AssociationTransaction)], incoming: List[(Int, AssociationTransaction)])
}
