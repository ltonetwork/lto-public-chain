package com.ltonetwork.state.reader

import cats.implicits._
import com.ltonetwork.account.Address
import com.ltonetwork.block.Block.BlockId
import com.ltonetwork.block.{Block, BlockHeader}
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.lease.LeaseTransaction
import com.ltonetwork.transaction.smart.script.Script

class CompositeBlockchain(inner: Blockchain, maybeDiff: => Option[Diff], carry: Long) extends Blockchain {

  private def diff = maybeDiff.getOrElse(Diff.empty)

  override def portfolio(a: Address): Portfolio = inner.portfolio(a).combine(diff.portfolios.getOrElse(a, Portfolio.empty))

  override def balance(address: Address): Long =
    inner.balance(address) + diff.portfolios.getOrElse(address, Portfolio.empty).balance

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = {
    inner.leaseDetails(leaseId).map(ld => ld.copy(isActive = diff.leaseState.getOrElse(leaseId, ld.isActive))) orElse
      diff.transactions.get(leaseId).collect {
        case (h, lt: LeaseTransaction, _) =>
          LeaseDetails(lt.sender, lt.recipient, h, lt.amount, diff.leaseState(lt.id()))
      }
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] =
    diff.transactions
      .get(id)
      .map(t => (t._1, t._2))
      .orElse(inner.transactionInfo(id))

  override def transactionHeight(id: ByteStr): Option[Int] =
    diff.transactions
      .get(id)
      .map(_._1)
      .orElse(inner.transactionHeight(id))

  override def height: Int = inner.height + (if (maybeDiff.isDefined) 1 else 0)

  override def addressTransactions(address: Address, types: Set[Byte], count: Int, from: Int): Seq[(Int, Transaction)] = {
    val transactionsFromDiff = diff.transactions.values.view
      .collect {
        case (height, tx, addresses) if addresses(address) && (types.isEmpty || types.contains(tx.builder.typeId)) => (height, tx)
      }
      .slice(from, from + count)
      .toSeq

    val actualTxCount = transactionsFromDiff.length

    if (actualTxCount == count) transactionsFromDiff
    else {
      transactionsFromDiff ++ inner.addressTransactions(address, types, count - actualTxCount, 0)
    }
  }

  override def allActiveLeases: Set[LeaseTransaction] = {
    val (active, canceled) = diff.leaseState.partition(_._2)
    val fromDiff = active.keys
      .map { id =>
        diff.transactions(id)._2
      }
      .collect { case lt: LeaseTransaction => lt }
      .toSet
    val fromInner = inner.allActiveLeases.filterNot(ltx => canceled.keySet.contains(ltx.id()))
    fromDiff ++ fromInner
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = {
    val b = Map.newBuilder[Address, A]
    for ((a, p) <- diff.portfolios if p.lease != LeaseBalance.empty || p.balance != 0) {
      pf.runWith(b += a -> _)(a -> portfolio(a).copy())
    }

    inner.collectLposPortfolios(pf) ++ b.result()
  }

  override def containsTransaction(id: ByteStr): Boolean = diff.transactions.contains(id) || inner.containsTransaction(id)

  override def forgetTransactions(pred: (AssetId, Long) => Boolean) = inner.forgetTransactions(pred)
  override def learnTransactions(values: Map[AssetId, Long]): Unit  = inner.learnTransactions(values)

  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = {
    if (inner.heightOf(to).isDefined || maybeDiff.isEmpty) {
      inner.balanceSnapshots(address, from, to)
    } else {
      val bs = BalanceSnapshot(height, portfolio(address))
      if (inner.height > 0 && from < this.height) bs +: inner.balanceSnapshots(address, from, to) else Seq(bs)
    }
  }

  override def accountScript(address: Address): Option[Script] = {
    diff.scripts.get(address) match {
      case None            => inner.accountScript(address)
      case Some(None)      => None
      case Some(Some(scr)) => Some(scr)
    }
  }

  override def hasScript(address: Address): Boolean = {
    diff.scripts.get(address) match {
      case None          => inner.hasScript(address)
      case Some(None)    => false
      case Some(Some(_)) => true
    }
  }

  override def accountData(acc: Address): AccountDataInfo = {
    val fromInner = inner.accountData(acc)
    val fromDiff  = diff.accountData.get(acc).orEmpty
    fromInner.combine(fromDiff)
  }

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = {
    val diffData = diff.accountData.get(acc).orEmpty
    diffData.data.get(key).orElse(inner.accountData(acc, key))
  }

  private def changedBalances(pred: Portfolio => Boolean, f: Address => Long): Map[Address, Long] =
    for {
      (address, p) <- diff.portfolios
      if pred(p)
    } yield address -> f(address)

  override def ltoDistribution(height: Int): Map[Address, Long] = {
    val innerDistribution = inner.ltoDistribution(height)
    if (height < this.height) innerDistribution
    else {
      innerDistribution ++ changedBalances(_.balance != 0, portfolio(_).balance)
    }
  }

  override def score: BigInt = inner.score

  override def scoreOf(blockId: ByteStr): Option[BigInt] = inner.scoreOf(blockId)

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = inner.blockHeaderAndSize(height)

  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = inner.blockHeaderAndSize(blockId)

  override def lastBlock: Option[Block] = inner.lastBlock

  override def carryFee: Long = carry

  override def blockBytes(height: Int): Option[Array[Byte]] = inner.blockBytes(height)

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] = inner.blockBytes(blockId)

  override def heightOf(blockId: ByteStr): Option[Int] = inner.heightOf(blockId)

  /** Returns the most recent block IDs, starting from the most recent  one */
  override def lastBlockIds(howMany: Int): Seq[ByteStr] = inner.lastBlockIds(howMany)

  /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = inner.blockIdsAfter(parentSignature, howMany)

  override def parent(block: Block, back: Int): Option[Block] = inner.parent(block, back)

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = inner.approvedFeatures

  override def activatedFeatures: Map[Short, Int] = inner.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = inner.featureVotes(height)

  /** Fee vote related */
  override def feePrice(height: Int): Long = inner.feePrice(height)

  override def feeVotes(height: Int): Int = inner.feeVotes(height)

  override def append(diff: Diff, carryFee: Long, block: Block): Unit = inner.append(diff, carryFee, block)

  override def rollbackTo(targetBlockId: ByteStr): Seq[Block] = inner.rollbackTo(targetBlockId)

  override def associations(address: Address): Blockchain.Associations = {
    val a0 = inner.associations(address)
    val diffAssociations: Seq[(Int, AssociationTransaction)] = maybeDiff
      .map(
        d =>
          d.transactions.values
            .map(i => (i._1, i._2))
            .filter(x => {
              val tpid = x._2.builder.typeId
              tpid == IssueAssociationTransaction.typeId || tpid == RevokeAssociationTransaction.typeId
            })
            .toList
            .map(_.asInstanceOf[(Int, AssociationTransaction)]))
      .getOrElse(List.empty)
    val outgoing = diffAssociations.filter(_._2.sender.toAddress == address)
    val incoming = diffAssociations.filter(_._2.recipient == address)

    Blockchain.Associations(a0.outgoing ++ outgoing, a0.incoming ++ incoming)
  }

  override def sponsorOf(address: Address): List[Address] =
    maybeDiff
      .map(d =>
        d.sponsoredBy.get(address) match {
          case Some(list) => list
          case None       => inner.sponsorOf(address)
      })
      .getOrElse(inner.sponsorOf(address))
}

object CompositeBlockchain {
  def composite(inner: Blockchain, diff: Diff, carryFee: Long): Blockchain = new CompositeBlockchain(inner, Some(diff), carryFee)
}
