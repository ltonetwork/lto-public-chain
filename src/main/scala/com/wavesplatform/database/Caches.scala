package com.wavesplatform.database

import java.util

import cats.syntax.monoid._
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.state.{_}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.{AssetId, AssociationTransaction, Transaction}
import com.wavesplatform.utils.ScorexLogging

import scala.collection.JavaConverters._

trait Caches extends Blockchain with ScorexLogging {

  import Caches._

  protected def maxCacheSize: Int

  @volatile
  private var current = (loadHeight(), loadScore(), loadLastBlock())

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

  protected def loadBalance(req: (Address, Option[AssetId])): Long
  protected def loadLeaseBalance(address: Address): LeaseBalance

  private val portfolioCache: LoadingCache[Address, Portfolio] = cache(maxCacheSize, loadPortfolio)
  protected def loadPortfolio(address: Address): Portfolio
  protected def discardPortfolio(address: Address): Unit = portfolioCache.invalidate(address)
  override def portfolio(a: Address): Portfolio          = portfolioCache.get(a)

  private val volumeAndFeeCache: LoadingCache[ByteStr, VolumeAndFee] = cache(maxCacheSize, loadVolumeAndFee)
  protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee
  protected def discardVolumeAndFee(orderId: ByteStr): Unit       = volumeAndFeeCache.invalidate(orderId)
  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = volumeAndFeeCache.get(orderId)

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

  protected def doAppend(block: Block,
                         carry: Long,
                         newAddresses: Map[Address, BigInt],
                         wavesBalances: Map[BigInt, Long],
                         assetBalances: Map[BigInt, Map[ByteStr, Long]],
                         leaseBalances: Map[BigInt, LeaseBalance],
                         addressTransactions: Map[BigInt, List[ByteStr]],
                         leaseStates: Map[ByteStr, Boolean],
                         transactions: Map[ByteStr, (Transaction, Set[BigInt])],
                         reissuedAssets: Map[ByteStr, AssetInfo],
                         filledQuantity: Map[ByteStr, VolumeAndFee],
                         scripts: Map[BigInt, Option[Script]],
                         data: Map[BigInt, AccountDataInfo],
                         aliases: Map[Alias, BigInt],
                         sponsorship: Map[AssetId, Sponsorship],
                         assocs: List[(Int, AssociationTransaction)]): Unit

  override def append(diff: Diff, carryFee: Long, block: Block): Unit = {
    val newHeight = current._1 + 1

    val newAddresses = Set.newBuilder[Address]
    newAddresses ++= diff.portfolios.keys.filter(addressIdCache.get(_).isEmpty)
    for ((_, _, addresses) <- diff.transactions.values; address <- addresses if addressIdCache.get(address).isEmpty) {
      newAddresses += address
    }

    val newAddressIds = (for {
      (address, offset) <- newAddresses.result().zipWithIndex
    } yield address -> (lastAddressId + offset + 1)).toMap

    def addressId(address: Address): BigInt = (newAddressIds.get(address) orElse addressIdCache.get(address)).get

    lastAddressId += newAddressIds.size

    log.trace(s"CACHE newAddressIds = $newAddressIds")
    log.trace(s"CACHE lastAddressId = $lastAddressId")

    val wavesBalances        = Map.newBuilder[BigInt, Long]
    val assetBalances        = Map.newBuilder[BigInt, Map[ByteStr, Long]]
    val leaseBalances        = Map.newBuilder[BigInt, LeaseBalance]
    val updatedLeaseBalances = Map.newBuilder[Address, LeaseBalance]
    val newPortfolios        = Seq.newBuilder[Address]
    val newBalances          = Map.newBuilder[(Address, Option[AssetId]), java.lang.Long]

    for ((address, portfolioDiff) <- diff.portfolios) {
      val aid = addressId(address)
      if (portfolioDiff.balance != 0) {
        val wbalance = (portfolioDiff.balance + loadBalance(address, None))
        wavesBalances += aid           -> wbalance
        newBalances += (address, None) -> wbalance
      }

      if (portfolioDiff.lease != LeaseBalance.empty) {
        val lease = loadLeaseBalance(address).combine(portfolioDiff.lease)
        leaseBalances += aid            -> lease
        updatedLeaseBalances += address -> lease
      }

      newPortfolios += address
    }

    val newFills = for {
      (orderId, fillInfo) <- diff.orderFills
    } yield orderId -> volumeAndFeeCache.get(orderId).combine(fillInfo)

    val addressTransactions: Map[BigInt, List[ByteStr]] =
      diff.transactions.toList
        .flatMap {
          case (_, (h, tx, addrs)) =>
            addrs.map { addr =>
              val addrId = addressId(addr)
              val htx    = (h, tx)
              addrId -> htx
            }
        }
        .groupBy(_._1)
        .mapValues { txs =>
          val sorted = txs.sortBy { case (_, (h, tx)) => (-h, -tx.timestamp) }
          sorted.map { case (_, (_, tx)) => tx.id() }
        }

    val newTransactions = Map.newBuilder[ByteStr, (Transaction, Set[BigInt])]
    for ((id, (_, tx, addresses)) <- diff.transactions) {
      transactionIds.put(id, tx.timestamp)
      newTransactions += id -> ((tx, addresses.map(addressId)))
    }

    val newAssociations: List[(Int, AssociationTransaction)] = diff.transactions.values
      .filter(_._2.builder.typeId == AssociationTransaction.typeId)
      .map(x => (x._1, x._2.asInstanceOf[AssociationTransaction]))
      .toList

    current = (newHeight, current._2 + block.blockScore(), Some(block))

    doAppend(
      block,
      carryFee,
      newAddressIds,
      wavesBalances.result(),
      assetBalances.result(),
      leaseBalances.result(),
      addressTransactions,
      diff.leaseState,
      newTransactions.result(),
      diff.issuedAssets,
      newFills,
      diff.scripts.map { case (address, s)        => addressId(address) -> s },
      diff.accountData.map { case (address, data) => addressId(address) -> data },
      diff.aliases.map { case (a, address)        => a                  -> addressId(address) },
      diff.sponsorship,
      newAssociations
    )

    for ((address, id)           <- newAddressIds) addressIdCache.put(address, Some(id))
    for ((orderId, volumeAndFee) <- newFills) volumeAndFeeCache.put(orderId, volumeAndFee)
    for (address                 <- newPortfolios.result()) portfolioCache.invalidate(address)
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
        override def load(key: K) = loader(key)
      })
}
