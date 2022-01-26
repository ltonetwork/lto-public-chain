package com.ltonetwork.database

import com.google.common.cache.CacheBuilder
import com.ltonetwork.account.Address
import com.ltonetwork.block.{Block, BlockHeader}
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state._
import com.ltonetwork.state.reader.LeaseDetails
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.sponsorship.SponsorshipTransactionBase
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.ScorexLogging
import org.iq80.leveldb.{DB, ReadOptions}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

object LevelDBWriter {
  private def loadLeaseStatus(db: ReadOnlyDB, leaseId: ByteStr): Boolean =
    db.get(Keys.leaseStatusHistory(leaseId)).headOption.fold(false)(h => db.get(Keys.leaseStatus(leaseId)(h)))

  /** {{{
    * ([10, 7, 4], 5, 11) => [10, 7, 4]
    * ([10, 7], 5, 11) => [10, 7, 1]
    * }}}
    */
  private[database] def slice(v: Seq[Int], from: Int, to: Int): Seq[Int] = {
    val (c1, c2) = v.dropWhile(_ > to).partition(_ > from)
    c1 :+ c2.headOption.getOrElse(1)
  }

  /**
    * @todo move this method to `object LevelDBWriter` once SmartAccountTrading is activated
    */
  private[database] def merge(wbh: Seq[Int], lbh: Seq[Int]): Seq[(Int, Int)] = {

    /**
      * Fixed implementation where
      * {{{([15, 12, 3], [12, 5]) => [(15, 12), (12, 12), (3, 5)]}}}
      */
    @tailrec
    def recMergeFixed(wh: Int, wt: Seq[Int], lh: Int, lt: Seq[Int], buf: ArrayBuffer[(Int, Int)]): ArrayBuffer[(Int, Int)] = {
      buf += wh -> lh
      if (wt.isEmpty && lt.isEmpty) {
        buf
      } else if (wt.isEmpty) {
        recMergeFixed(wh, wt, lt.head, lt.tail, buf)
      } else if (lt.isEmpty) {
        recMergeFixed(wt.head, wt.tail, lh, lt, buf)
      } else {
        if (wh == lh) {
          recMergeFixed(wt.head, wt.tail, lt.head, lt.tail, buf)
        } else if (wh > lh) {
          recMergeFixed(wt.head, wt.tail, lh, lt, buf)
        } else {
          recMergeFixed(wh, wt, lt.head, lt.tail, buf)
        }
      }
    }
    recMergeFixed(wbh.head, wbh.tail, lbh.head, lbh.tail, ArrayBuffer.empty)
  }

  implicit class ReadOnlyDBExt(val db: ReadOnlyDB) extends AnyVal {
    def fromHistory[A](historyKey: Key[Seq[Int]], valueKey: Int => Key[A]): Option[A] =
      for {
        lastChange <- db.get(historyKey).headOption
      } yield db.get(valueKey(lastChange))

    def hasInHistory(historyKey: Key[Seq[Int]], v: Int => Key[_]): Boolean =
      db.get(historyKey)
        .headOption
        .exists(h => db.has(v(h)))
  }

  implicit class RWExt(val db: RW) extends AnyVal {
    def fromHistory[A](historyKey: Key[Seq[Int]], valueKey: Int => Key[A]): Option[A] =
      for {
        lastChange <- db.get(historyKey).headOption
      } yield db.get(valueKey(lastChange))
  }
}

class LevelDBWriter(writableDB: DB, fs: FunctionalitySettings, val maxCacheSize: Int = 100000, val maxRollbackDepth: Int = 2000)
    extends Caches
    with ScorexLogging {

  import LevelDBWriter._

  private def readOnly[A](f: ReadOnlyDB => A): A = {
    val s = writableDB.getSnapshot
    try f(new ReadOnlyDB(writableDB, new ReadOptions().snapshot(s)))
    finally s.close()
  }

  private def readWrite[A](f: RW => A): A = {
    val rw = new RW(writableDB)
    try f(rw)
    finally rw.close()
  }

  override protected def loadMaxAddressId(): BigInt = readOnly(db => db.get(Keys.lastAddressId).getOrElse(BigInt(0)))

  override protected def loadAddressId(address: Address): Option[BigInt] = readOnly(db => db.get(Keys.addressId(address)))

  override protected def loadHeight(): Int = readOnly(_.get(Keys.height))

  override protected def loadScore(): BigInt = readOnly(db => db.get(Keys.score(db.get(Keys.height))))

  override protected def loadLastBlock(): Option[Block] = readOnly(db => db.get(Keys.blockAt(db.get(Keys.height))))

  override protected def loadScript(address: Address): Option[Script] = readOnly { db =>
    addressId(address).fold(Option.empty[Script]) { addressId =>
      db.fromHistory(Keys.addressScriptHistory(addressId), Keys.addressScript(addressId)).flatten
    }
  }

  override protected def hasScriptBytes(address: Address): Boolean = readOnly { db =>
    addressId(address).fold(false) { addressId =>
      db.hasInHistory(Keys.addressScriptHistory(addressId), Keys.addressScript(addressId))
    }
  }
  override def sponsorOf(address: Address): List[Address] = readOnly { db =>
    addressId(address).fold(List.empty[Address]) { addressId =>
      db.fromHistory(Keys.sponsorshipHistory(addressId), Keys.sponsorshipStatus(addressId)).getOrElse(List.empty)
    }
  }

  override def carryFee: Long = readOnly(_.get(Keys.carryFee(height)))

  override def accountData(address: Address): AccountDataInfo = readOnly { db =>
    AccountDataInfo((for {
      addressId <- addressId(address).toSeq
      keyChunkCount = db.get(Keys.dataKeyChunkCount(addressId))
      chunkNo <- Range(0, keyChunkCount)
      key     <- db.get(Keys.dataKeyChunk(addressId, chunkNo))
      value   <- accountData(address, key)
    } yield key -> value).toMap)
  }

  override def accountData(address: Address, key: String): Option[DataEntry[_]] = readOnly { db =>
    addressId(address).fold(Option.empty[DataEntry[_]]) { addressId =>
      db.fromHistory(Keys.dataHistory(addressId, key), Keys.data(addressId, key)).flatten
    }
  }

  override def balance(address: Address): Long = readOnly { db =>
    addressId(address).fold(0L) { addressId =>
      db.fromHistory(Keys.ltoBalanceHistory(addressId), Keys.ltoBalance(addressId)).getOrElse(0L)
    }
  }

  private def loadLposPortfolio(db: ReadOnlyDB, addressId: BigInt) = Portfolio(
    db.fromHistory(Keys.ltoBalanceHistory(addressId), Keys.ltoBalance(addressId)).getOrElse(0L),
    db.fromHistory(Keys.leaseBalanceHistory(addressId), Keys.leaseBalance(addressId)).getOrElse(LeaseBalance.empty)
  )

  private def loadPortfolio(db: ReadOnlyDB, addressId: BigInt) = loadLposPortfolio(db, addressId).copy()

  override protected def loadPortfolio(address: Address): Portfolio = readOnly { db =>
    addressId(address).fold(Portfolio.empty)(loadPortfolio(db, _))
  }

  override protected def loadApprovedFeatures(): Map[Short, Int] = readOnly(_.get(Keys.approvedFeatures))

  override protected def loadActivatedFeatures(): Map[Short, Int] = readOnly(_.get(Keys.activatedFeatures)) ++ fs.preActivatedFeatures

  private def updateHistory(rw: RW, key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] =
    updateHistory(rw, rw.get(key), key, threshold, kf)

  private def updateHistory(rw: RW, history: Seq[Int], key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] = {
    val (c1, c2) = history.partition(_ > threshold)
    rw.put(key, (height +: c1) ++ c2.headOption)
    c2.drop(1).map(kf(_).keyBytes)
  }

  override protected def doAppend(carry: Long,
                                  block: Block,
                                  newAddresses: Map[Address, BigInt],
                                  ltoBalances: Map[BigInt, Long],
                                  leaseBalances: Map[BigInt, LeaseBalance],
                                  leaseStates: Map[ByteStr, Boolean],
                                  transactions: Map[ByteStr, (Transaction, Set[BigInt])],
                                  addressTransactions: Map[BigInt, List[(Int, ByteStr)]],
                                  scripts: Map[BigInt, Option[Script]],
                                  data: Map[BigInt, AccountDataInfo],
                                  assocs: List[(Int, AssociationTransaction)],
                                  sponsorship: Map[BigInt, List[Address]]): Unit = readWrite { rw =>
    val expiredKeys = new ArrayBuffer[Array[Byte]]

    rw.put(Keys.height, height)
    rw.put(Keys.blockAt(height), Some(block))
    rw.put(Keys.heightOf(block.uniqueId), Some(height))
    rw.put(Keys.lastAddressId, Some(loadMaxAddressId() + newAddresses.size))
    rw.put(Keys.score(height), rw.get(Keys.score(height - 1)) + block.blockScore())

    for ((address, id) <- newAddresses) {
      rw.put(Keys.addressId(address), Some(id))
      rw.put(Keys.idToAddress(id), address)
    }

    val threshold = height - maxRollbackDepth

    val newAddressesForLto = ArrayBuffer.empty[BigInt]
    val updatedBalanceAddresses = for ((addressId, balance) <- ltoBalances) yield {
      val kwbh = Keys.ltoBalanceHistory(addressId)
      val wbh  = rw.get(kwbh)
      if (wbh.isEmpty) {
        newAddressesForLto += addressId
      }
      rw.put(Keys.ltoBalance(addressId)(height), balance)
      expiredKeys ++= updateHistory(rw, wbh, kwbh, threshold, Keys.ltoBalance(addressId))
      addressId
    }

    val changedAddresses = addressTransactions.keys ++ updatedBalanceAddresses

    if (newAddressesForLto.nonEmpty) {
      val newSeqNr = rw.get(Keys.addressesForLtoSeqNr) + 1
      rw.put(Keys.addressesForLtoSeqNr, newSeqNr)
      rw.put(Keys.addressesForLto(newSeqNr), newAddressesForLto)
    }

    for ((addressId, leaseBalance) <- leaseBalances) {
      rw.put(Keys.leaseBalance(addressId)(height), leaseBalance)
      expiredKeys ++= updateHistory(rw, Keys.leaseBalanceHistory(addressId), threshold, Keys.leaseBalance(addressId))
    }

    rw.put(Keys.changedAddresses(height), changedAddresses.toSeq)

    for ((leaseId, state) <- leaseStates) {
      rw.put(Keys.leaseStatus(leaseId)(height), state)
      expiredKeys ++= updateHistory(rw, Keys.leaseStatusHistory(leaseId), threshold, Keys.leaseStatus(leaseId))
    }

    for ((addressId, script) <- scripts) {
      expiredKeys ++= updateHistory(rw, Keys.addressScriptHistory(addressId), threshold, Keys.addressScript(addressId))
      script.foreach(s => rw.put(Keys.addressScript(addressId)(height), Some(s)))
    }

    for ((addressId, newSponsorList) <- sponsorship) {
      expiredKeys ++= updateHistory(rw, Keys.sponsorshipHistory(addressId), threshold, Keys.sponsorshipStatus(addressId))
      rw.put(Keys.sponsorshipStatus(addressId)(height), newSponsorList)
    }

    for ((addressId, addressData) <- data) {
      val newKeys = (
        for {
          (key, value) <- addressData.data
          kdh   = Keys.dataHistory(addressId, key)
          isNew = rw.get(kdh).isEmpty
          _     = rw.put(Keys.data(addressId, key)(height), Some(value))
          _     = expiredKeys ++= updateHistory(rw, kdh, threshold, Keys.data(addressId, key))
          if isNew
        } yield key
      ).toSeq
      if (newKeys.nonEmpty) {
        val chunkCountKey = Keys.dataKeyChunkCount(addressId)
        val chunkCount    = rw.get(chunkCountKey)
        rw.put(Keys.dataKeyChunk(addressId, chunkCount), newKeys)
        rw.put(chunkCountKey, chunkCount + 1)
      }
    }

    for ((addressId, txs) <- addressTransactions) {
      val kk                          = Keys.addressTransactionSeqNr(addressId)
      val nextSeqNr                   = rw.get(kk) + 1
      val t: Key[Seq[(Int, AssetId)]] = Keys.addressTransactionIds(addressId, nextSeqNr)
      rw.put(t, txs)
      rw.put(kk, nextSeqNr)
    }

    def f(addr: Address, txs: Seq[AssociationTransaction], seqNrKey: ByteStr => Key[Int], idKey: (ByteStr, Int) => Key[Array[Byte]]) = {
      val curSeq: Key[Int] = seqNrKey(addr.bytes)
      val last             = rw.get(curSeq)
      txs.zipWithIndex
        .map { case (tx, nr) => (tx, nr + last + 1) }
        .foreach {
          case (tx, nr) =>
            val t: Key[Array[Byte]] = idKey(addr.bytes, nr)
            rw.put(t.keyBytes, tx.id().arr)
        }
      rw.put(curSeq, last + txs.size)
    }

    assocs.map(_._2).groupBy(_.sender.toAddress).foreach {
      case (addr, txs) => f(addr, txs, Keys.outgoingAssociationsSeqNr, Keys.outgoingAssociationTransactionId)
    }

    assocs.map(_._2).groupBy(_.recipient).foreach {
      case (addr, txs) => f(addr, txs, Keys.incomingAssociationsSeqNr, Keys.incomingAssociationTransactionId)
    }

    for ((id, (tx, _)) <- transactions) {
      rw.put(Keys.transactionInfo(id), Some((height, tx)))
    }

    val activationWindowSize = fs.activationWindowSize(height)
    if (height % activationWindowSize == 0) {
      val minVotes = fs.blocksForFeatureActivation(height)
      val newlyApprovedFeatures = featureVotes(height).collect {
        case (featureId, voteCount) if voteCount + (if (block.featureVotes(featureId)) 1 else 0) >= minVotes => featureId -> height
      }

      if ((newlyApprovedFeatures -- fs.preActivatedFeatures.keySet).nonEmpty) {
        approvedFeaturesCache = newlyApprovedFeatures ++ rw.get(Keys.approvedFeatures)
        rw.put(Keys.approvedFeatures, approvedFeaturesCache)

        val featuresToSave = newlyApprovedFeatures.mapValues(_ + activationWindowSize) ++ rw.get(Keys.activatedFeatures)

        activatedFeaturesCache = featuresToSave ++ fs.preActivatedFeatures
        rw.put(Keys.activatedFeatures, featuresToSave)
      }
    }

    rw.put(Keys.transactionIdsAtHeight(height), transactions.keys.toSeq)
    rw.put(Keys.carryFee(height), carry)
    expiredKeys.foreach(rw.delete)

  }

  override protected def doRollback(targetBlockId: ByteStr): Seq[Block] = {
    readOnly(_.get(Keys.heightOf(targetBlockId))).fold(Seq.empty[Block]) { targetHeight =>
      log.debug(s"Rolling back to block $targetBlockId at $targetHeight")

      val discardedBlocks: Seq[Block] = for (currentHeight <- height until targetHeight by -1) yield {
        val portfoliosToInvalidate = Seq.newBuilder[Address]
        val scriptsToDiscard       = Seq.newBuilder[Address]

        // association transcations are discarded naturally.
        // association records are not discarded, but since
        // we query assocs as
        // tx <- transactionInfo(ByteStr(txId)).toList
        // the output will be consistent

        val discardedBlock = readWrite { rw =>
          log.trace(s"Rolling back to ${currentHeight - 1}")
          rw.put(Keys.height, currentHeight - 1)

          for (addressId <- rw.get(Keys.changedAddresses(currentHeight))) {
            val address = rw.get(Keys.idToAddress(addressId))

            rw.delete(Keys.ltoBalance(addressId)(currentHeight))
            rw.filterHistory(Keys.ltoBalanceHistory(addressId), currentHeight)

            rw.delete(Keys.leaseBalance(addressId)(currentHeight))
            rw.filterHistory(Keys.leaseBalanceHistory(addressId), currentHeight)

            log.trace(s"Discarding portfolio for $address")

            portfoliosToInvalidate += address
            balanceAtHeightCache.invalidate((currentHeight, addressId))
            leaseBalanceAtHeightCache.invalidate((currentHeight, addressId))

            val kTxSeqNr = Keys.addressTransactionSeqNr(addressId)
            val txSeqNr  = rw.get(kTxSeqNr)
            val kTxIds   = Keys.addressTransactionIds(addressId, txSeqNr)
            for ((_, id) <- rw.get(kTxIds).headOption; (h, _) <- rw.get(Keys.transactionInfo(id)) if h == currentHeight) {
              rw.delete(kTxIds)
              rw.put(kTxSeqNr, (txSeqNr - 1).max(0))
            }
          }

          val txIdsAtHeight = Keys.transactionIdsAtHeight(currentHeight)
          for (txId <- rw.get(txIdsAtHeight)) {
            forgetTransaction(txId)
            val ktxId = Keys.transactionInfo(txId)

            for ((_, tx) <- rw.get(ktxId)) {
              tx match {
                case _: GenesisTransaction                               => // genesis transaction can not be rolled back
                case _: TransferTransaction | _: MassTransferTransaction => // balances already restored

                case tx: LeaseTransaction =>
                  rollbackLeaseStatus(rw, tx.id(), currentHeight)
                case tx: CancelLeaseTransaction =>
                  rollbackLeaseStatus(rw, tx.leaseId, currentHeight)

                case tx: SponsorshipTransactionBase =>
                  rollbackSponsorshipStatus(rw, tx.recipient, currentHeight)

                case tx: SetScriptTransaction =>
                  val address = tx.sender.toAddress
                  scriptsToDiscard += address
                  for (addressId <- addressId(address)) {
                    rw.delete(Keys.addressScript(addressId)(currentHeight))
                    rw.filterHistory(Keys.addressScriptHistory(addressId), currentHeight)
                  }

                case tx: DataTransaction =>
                  val address = tx.sender.toAddress
                  for (addressId <- addressId(address)) {
                    tx.data.foreach { e =>
                      log.trace(s"Discarding ${e.key} for $address at $currentHeight")
                      rw.delete(Keys.data(addressId, e.key)(currentHeight))
                      rw.filterHistory(Keys.dataHistory(addressId, e.key), currentHeight)
                    }
                  }

                case tx: IssueAssociationTransaction  => // TODO
                case tx: RevokeAssociationTransaction => // TODO

                case _ => // do nothinhg specific
              }
            }

            rw.delete(ktxId)
          }

          rw.delete(txIdsAtHeight)

          val discardedBlock = rw
            .get(Keys.blockAt(currentHeight))
            .getOrElse(throw new IllegalArgumentException(s"No block at height $currentHeight"))

          rw.delete(Keys.blockAt(currentHeight))
          rw.delete(Keys.heightOf(discardedBlock.uniqueId))

          discardedBlock
        }

        portfoliosToInvalidate.result().foreach(discardPortfolio)
        scriptsToDiscard.result().foreach(discardScript)
        discardedBlock
      }

      log.debug(s"Rollback to block $targetBlockId at $targetHeight completed")

      discardedBlocks.reverse
    }
  }

  private def rollbackLeaseStatus(rw: RW, leaseId: ByteStr, currentHeight: Int): Unit = {
    rw.delete(Keys.leaseStatus(leaseId)(currentHeight))
    rw.filterHistory(Keys.leaseStatusHistory(leaseId), currentHeight)
  }

  private def rollbackSponsorshipStatus(rw: RW, of: Address, currentHeight: Int): Unit = {
    addressId(of).foreach { sponsoree =>
      rw.delete(Keys.sponsorshipStatus(sponsoree)(currentHeight))
      rw.filterHistory(Keys.sponsorshipHistory(sponsoree), currentHeight)
    }
  }
  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = readOnly(db => db.get(Keys.transactionInfo(id)))

  override def transactionHeight(id: ByteStr): Option[Int] = readOnly(db => db.get(Keys.transactionHeight(id)))

  override def addressTransactions(address: Address, types: Set[Byte], count: Int, from: Int): Seq[(Int, Transaction)] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq.empty[(Int, Transaction)]) { addressId =>
      val txs = for {
        seqNr          <- (db.get(Keys.addressTransactionSeqNr(addressId)) to 1 by -1).view
        (txType, txId) <- db.get(Keys.addressTransactionIds(addressId, seqNr))
        if types.isEmpty || types.contains(txType.toByte)
        (h, tx) <- db.get(Keys.transactionInfo(txId))
      } yield (h, tx)

      txs.slice(from, count).force
    }
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = readOnly { db =>
    db.get(Keys.transactionInfo(leaseId)) match {
      case Some((h, lt: LeaseTransaction)) =>
        Some(LeaseDetails(lt.sender, lt.recipient, h, lt.amount, loadLeaseStatus(db, leaseId)))
      case _ => None
    }
  }

  // These two caches are used exclusively for balance snapshots. They are not used for portfolios, because there aren't
  // as many miners, so snapshots will rarely be evicted due to overflows.

  private val balanceAtHeightCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .recordStats()
    .build[(Int, BigInt), java.lang.Long]()

  private val leaseBalanceAtHeightCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .recordStats()
    .build[(Int, BigInt), LeaseBalance]()

  override def balanceSnapshots(address: Address, from: Int, to: ByteStr): Seq[BalanceSnapshot] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq(BalanceSnapshot(1, 0, 0, 0))) { addressId =>
      val toHeight = this.heightOf(to).getOrElse(this.height)
      val ints     = db.get(Keys.ltoBalanceHistory(addressId))
      val wbh      = slice(ints, from, toHeight)
      val lbh      = slice(db.get(Keys.leaseBalanceHistory(addressId)), from, toHeight)
      for {
        (wh, lh) <- merge(wbh, lbh)
        wb = balanceAtHeightCache.get((wh, addressId), () => db.get(Keys.ltoBalance(addressId)(wh)))
        lb = leaseBalanceAtHeightCache.get((lh, addressId), () => db.get(Keys.leaseBalance(addressId)(lh)))
      } yield BalanceSnapshot(wh.max(lh), wb, lb.in, lb.out)
    }
  }

  override def allActiveLeases: Set[LeaseTransaction] = readOnly { db =>
    val txs = for {
      h  <- 1 to db.get(Keys.height)
      id <- db.get(Keys.transactionIdsAtHeight(h))
      if loadLeaseStatus(db, id)
      (_, tx) <- db.get(Keys.transactionInfo(id))
    } yield tx

    txs.collect { case lt: LeaseTransaction => lt }.toSet
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]) = readOnly { db =>
    val b = Map.newBuilder[Address, A]
    for (id <- BigInt(1) to db.get(Keys.lastAddressId).getOrElse(BigInt(0))) {
      val address = db.get(Keys.idToAddress(id))
      pf.runWith(b += address -> _)(address -> loadLposPortfolio(db, id))
    }
    b.result()
  }

  override def scoreOf(blockId: ByteStr): Option[BigInt] = readOnly(db => db.get(Keys.heightOf(blockId)).map(h => db.get(Keys.score(h))))

  override def feePrice: Long = 10000
  override def feePrice(height: Int): Long = 10000

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = readOnly(_.get(Keys.blockHeader(height)))

  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] =
    readOnly(db => db.get(Keys.heightOf(blockId)).flatMap(h => db.get(Keys.blockHeader(h))))

  override def blockBytes(height: Int): Option[Array[Byte]] = readOnly(_.get(Keys.blockBytes(height)))

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] =
    readOnly(db => db.get(Keys.heightOf(blockId)).flatMap(h => db.get(Keys.blockBytes(h))))

  override def heightOf(blockId: ByteStr): Option[Int] = readOnly(_.get(Keys.heightOf(blockId)))

  override def lastBlockIds(howMany: Int): immutable.IndexedSeq[ByteStr] = readOnly { db =>
    // since this is called from outside of the main blockchain updater thread, instead of using cached height,
    // explicitly read height from storage to make this operation atomic.
    val currentHeight = db.get(Keys.height)
    (currentHeight until (currentHeight - howMany).max(0) by -1)
      .map(h => db.get(Keys.blockHeader(h)).get._1.signerData.signature)
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = readOnly { db =>
    db.get(Keys.heightOf(parentSignature)).map { parentHeight =>
      (parentHeight until (parentHeight + howMany))
        .flatMap { h =>
          db.get(Keys.blockHeader(h))
        }
        .map { b =>
          b._1.signerData.signature
        }
    }
  }

  override def parent(block: Block, back: Int): Option[Block] = readOnly { db =>
    db.get(Keys.heightOf(block.reference)).flatMap(h => db.get(Keys.blockAt(h - back + 1)))
  }

  override def featureVotes(height: Int): Map[Short, Int] = readOnly { db =>
    fs.activationWindow(height)
      .flatMap(h => db.get(Keys.blockHeader(h)).fold(Seq.empty[Short])(_._1.featureVotes.toSeq))
      .groupBy(identity)
      .mapValues(_.size)
  }

  override def feeVotes(height: Int): Int = readOnly { db =>
    fs.feeVoteWindow(height)
      .map(h => db.get(Keys.blockHeader(h)).fold(0)(_._1.feeVote))
      .sum
  }

  override def ltoDistribution(height: Int): Map[Address, Long] = readOnly { db =>
    (for {
      seqNr     <- (1 to db.get(Keys.addressesForLtoSeqNr)).par
      addressId <- db.get(Keys.addressesForLto(seqNr)).par
      history = db.get(Keys.ltoBalanceHistory(addressId))
      actualHeight <- history.partition(_ > height)._2.headOption
      balance = db.get(Keys.ltoBalance(addressId)(actualHeight))
      if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq
  }

  override def associations(address: Address): Blockchain.Associations = readOnly { db =>
    def f(seqNrKey: ByteStr => Key[Int], idKey: (ByteStr, Int) => Key[Array[Byte]]) =
      (1 to db.get(seqNrKey(address.bytes)))
        .map { seqNr =>
          db.get(idKey(address.bytes, seqNr))
        }
        .distinct
        .flatMap(txId => transactionInfo(ByteStr(txId)))
        .map(x => (x._1, x._2.asInstanceOf[AssociationTransaction]))
        .toList

    Blockchain.Associations(
      outgoing = f(Keys.outgoingAssociationsSeqNr, Keys.outgoingAssociationTransactionId),
      incoming = f(Keys.incomingAssociationsSeqNr, Keys.incomingAssociationTransactionId)
    )
  }
}
