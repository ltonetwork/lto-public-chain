package com.ltonetwork.database

import cats.syntax.monoid._
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.ltonetwork.account.Address
import com.ltonetwork.block.Block
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.smart.script.Script

import java.util
import java.lang.{Integer => JInt, Long => JLong}
import scala.collection.JavaConverters._

trait Caches extends Blockchain {
  import Caches._

  protected def maxCacheSize: Int

  @volatile
  private var heightCache = loadHeight()
  protected def loadHeight(): Int
  override def height: Int = heightCache

  @volatile
  private var scoreCache = loadScore()
  protected def loadScore(): BigInt
  override def score: BigInt = scoreCache

  @volatile
  private var lastBlockCache = loadLastBlock()
  protected def loadLastBlock(): Option[Block]
  override def lastBlock: Option[Block] = lastBlockCache

  private val transactionIds                                       = new util.HashMap[ByteStr, Long]()
  protected def forgetTransaction(id: ByteStr): Unit               = transactionIds.remove(id)
  override def containsTransaction(id: ByteStr): Boolean           = transactionIds.containsKey(id)
  override def learnTransactions(values: Map[ByteStr, Long]): Unit = transactionIds.putAll(values.asJava)
  override def forgetTransactions(pred: (ByteStr, Long) => Boolean): Map[ByteStr, Long] = {
    val removedTransactions = Map.newBuilder[ByteStr, Long]
    val iterator            = transactionIds.entrySet().iterator()
    while (iterator.hasNext) {
      val e = iterator.next()
      if (pred(e.getKey, e.getValue)) {
        removedTransactions += e.getKey -> e.getValue
        iterator.remove()
      }
    }
    removedTransactions.result()
  }

  private val portfolioCache: LoadingCache[Address, Portfolio] = cache(maxCacheSize, loadPortfolio)
  protected def loadPortfolio(address: Address): Portfolio
  protected def discardPortfolio(address: Address): Unit = portfolioCache.invalidate(address)
  override def portfolio(a: Address): Portfolio          = portfolioCache.get(a)

  private val scriptCache: LoadingCache[Address, Option[Script]] = cache(maxCacheSize, loadScript)
  protected def loadScript(address: Address): Option[Script]
  protected def hasScriptBytes(address: Address): Boolean
  protected def discardScript(address: Address): Unit = scriptCache.invalidate(address)

  override def accountScript(address: Address): Option[Script] = scriptCache.get(address)
  override def hasScript(address: Address): Boolean =
    Option(scriptCache.getIfPresent(address)).flatten.isDefined || hasScriptBytes(address)

  private var lastAddressId = loadMaxAddressId()
  protected def loadMaxAddressId(): BigInt

  private val addressIdCache: LoadingCache[Address, Option[BigInt]] = cache(maxCacheSize, loadAddressId)
  protected def loadAddressId(address: Address): Option[BigInt]
  protected def addressId(address: Address): Option[BigInt] = addressIdCache.get(address)

  @volatile
  protected var approvedFeaturesCache: Map[Short, Int] = loadApprovedFeatures()
  protected def loadApprovedFeatures(): Map[Short, Int]
  override def approvedFeatures: Map[Short, Int] = approvedFeaturesCache

  @volatile
  protected var activatedFeaturesCache: Map[Short, Int] = loadActivatedFeatures()
  protected def loadActivatedFeatures(): Map[Short, Int]
  override def activatedFeatures: Map[Short, Int] = activatedFeaturesCache

  // Using JInt and JLong because Int and Long are not AnyRef which is required by CacheBuilder
  protected val feePriceCache: LoadingCache[JInt, JLong] = cache(maxCacheSize, loadFeePrice)
  protected def feePriceHeight(height: Int): Int
  protected def loadFeePrice(height: Int): Long
  protected def loadFeePrice(height: JInt): JLong = loadFeePrice(height.intValue())
  override def feePrice(height: Int): Long = {
    val h = feePriceHeight(height)
    feePriceCache.get(h)
  }

  protected def doAppend(carryFee: Long,
                         block: Block,
                         addresses: Map[Address, BigInt],
                         ltoBalances: Map[BigInt, Long],
                         leaseBalances: Map[BigInt, LeaseBalance],
                         leaseStates: Map[ByteStr, Boolean],
                         transactions: Map[ByteStr, (Transaction, Set[BigInt])],
                         addressTransactions: Map[BigInt, List[(Int, ByteStr)]],
                         scripts: Map[BigInt, Option[Script]],
                         data: Map[BigInt, AccountDataInfo],
                         assocs: List[(Int, AssociationTransaction)],
                         sponsorship: Map[BigInt, List[Address]]): Unit

  override def append(diff: Diff, carryFee: Long, block: Block): Unit = {
    heightCache += 1
    scoreCache += block.blockScore()
    lastBlockCache = Some(block)

    val newAddresses = Set.newBuilder[Address]
    newAddresses ++= (diff.portfolios.keys ++ diff.sponsoredBy.keys).toSet.filter(addressIdCache.get(_).isEmpty)
    for ((_, _, addresses) <- diff.transactions.values; address <- addresses if addressIdCache.get(address).isEmpty) {
      newAddresses += address
    }

    val newAddressIds = (for {
      (address, offset) <- newAddresses.result().zipWithIndex
    } yield address -> (lastAddressId + offset + 1)).toMap

    def addressId(address: Address): BigInt = (newAddressIds.get(address) orElse addressIdCache.get(address)).get

    lastAddressId += newAddressIds.size

    val ltoBalances   = Map.newBuilder[BigInt, Long]
    val leaseBalances = Map.newBuilder[BigInt, LeaseBalance]
    val newPortfolios = Map.newBuilder[Address, Portfolio]

    for ((address, portfolioDiff) <- diff.portfolios) {
      val newPortfolio = portfolioCache.get(address).combine(portfolioDiff)
      if (portfolioDiff.balance != 0) {
        ltoBalances += addressId(address) -> newPortfolio.balance
      }

      if (portfolioDiff.lease != LeaseBalance.empty) {
        leaseBalances += addressId(address) -> newPortfolio.lease
      }

      newPortfolios += address -> newPortfolio
    }

    val newTransactions = Map.newBuilder[ByteStr, (Transaction, Set[BigInt])]
    for ((id, (_, tx, addresses)) <- diff.transactions) {
      transactionIds.put(id, tx.timestamp)
      newTransactions += id -> ((tx, addresses.map(addressId)))
    }

    val newAssociations: List[(Int, AssociationTransaction)] = diff.transactions.values
      .filter(x => {
        val tpid = x._2.builder.typeId
        tpid == IssueAssociationTransaction.typeId || tpid == RevokeAssociationTransaction.typeId
      })
      .map(x => (x._1, x._2.asInstanceOf[AssociationTransaction]))
      .toList

    doAppend(
      carryFee,
      block,
      newAddressIds,
      ltoBalances.result(),
      leaseBalances.result(),
      diff.leaseState,
      newTransactions.result(),
      diff.accountTransactionIds.map({ case (addr, txs) => addressId(addr)    -> txs }),
      diff.scripts.map { case (address, s)              => addressId(address) -> s },
      diff.accountData.map { case (address, data)       => addressId(address) -> data },
      newAssociations,
      diff.sponsoredBy.map { case (sponsoree, v) => (addressId(sponsoree) -> v) }
    )

    for ((address, id)        <- newAddressIds) addressIdCache.put(address, Some(id))
    for ((address, portfolio) <- newPortfolios.result()) portfolioCache.put(address, portfolio)
    scriptCache.putAll(diff.scripts.asJava)
  }

  protected def doRollback(targetBlockId: ByteStr): Seq[Block]

  override def rollbackTo(targetBlockId: ByteStr): Seq[Block] = {
    val discardedBlocks = doRollback(targetBlockId)

    heightCache = loadHeight()
    scoreCache = loadScore()
    lastBlockCache = loadLastBlock()

    activatedFeaturesCache = loadActivatedFeatures()
    approvedFeaturesCache = loadApprovedFeatures()

    discardedBlocks
  }
}

object Caches {
  def cache[K <: AnyRef, V <: AnyRef](maximumSize: Int, loader: K => V): LoadingCache[K, V] =
    CacheBuilder
      .newBuilder()
      .maximumSize(maximumSize)
      .build(new CacheLoader[K, V] {
        override def load(key: K): V = loader(key)
      })
}
