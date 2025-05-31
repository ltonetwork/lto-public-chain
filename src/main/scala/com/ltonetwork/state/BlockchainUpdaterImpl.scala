package com.ltonetwork.state

import cats.implicits._
import com.ltonetwork.account.Address
import com.ltonetwork.block.Block.BlockId
import com.ltonetwork.block.{Block, BlockHeader, MicroBlock}
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.features.FeatureProvider._
import com.ltonetwork.metrics.{Instrumented, TxsInBlockchainStats}
import com.ltonetwork.mining.{MiningConstraint, MiningConstraints, MultiDimensionalMiningConstraint}
import com.ltonetwork.settings.LtoSettings
import com.ltonetwork.state.diffs.BlockDiffer
import com.ltonetwork.state.reader.{CompositeBlockchain, LeaseDetails}
import com.ltonetwork.transaction.ValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.lease._
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import kamon.Kamon
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

class BlockchainUpdaterImpl(blockchain: Blockchain, settings: LtoSettings, time: Time)
    extends BlockchainUpdater
    with NG
    with ScorexLogging
    with Instrumented {

  import com.ltonetwork.state.BlockchainUpdaterImpl._
  import settings.blockchainSettings.functionalitySettings

  private lazy val maxBlockReadinessAge = settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis

  private var ngState: Option[NgState]              = Option.empty
  private var restTotalConstraint: MiningConstraint = MiningConstraints(settings.minerSettings, blockchain, blockchain.height).total

  private val service               = monix.execution.Scheduler.singleThread("last-block-info-publisher")
  private val internalLastBlockInfo = ConcurrentSubject.publish[LastBlockInfo](service)

  override def isLastBlockId(id: ByteStr): Boolean = ngState.exists(_.contains(id)) || lastBlock.exists(_.uniqueId == id)

  override val lastBlockInfo: Observable[LastBlockInfo] = internalLastBlockInfo.cache(1)
  lastBlockInfo.subscribe()(monix.execution.Scheduler.global) // Start caching

  def blockchainReady: Boolean = {
    val lastBlock = ngState.map(_.base.timestamp).orElse(blockchain.lastBlockTimestamp).get
    lastBlock + maxBlockReadinessAge > time.correctedTime()
  }

  // Store last block information in a cache
  lastBlockId.foreach { id =>
    internalLastBlockInfo.onNext(LastBlockInfo(id, height, score, blockchainReady))
  }

  private def displayFeatures(s: Set[Short]): String =
    s"FEATURE${if (s.size > 1) "S" else ""} ${s.mkString(", ")} ${if (s.size > 1) "have been" else "has been"}"

  private def featuresApprovedWithBlock(block: Block): Set[Short] = {
    val height = blockchain.height + 1

    val featuresCheckPeriod        = functionalitySettings.activationWindowSize(height)
    val blocksForFeatureActivation = functionalitySettings.blocksForFeatureActivation(height)

    if (height % featuresCheckPeriod == 0) {
      val approvedFeatures = blockchain
        .featureVotes(height)
        .map { case (feature, votes) => feature -> (if (block.featureVotes.contains(feature)) votes + 1 else votes) }
        .filter { case (_, votes) => votes >= blocksForFeatureActivation }
        .keySet

      if (approvedFeatures.nonEmpty) log.info(s"${displayFeatures(approvedFeatures)} APPROVED at height $height")

      val unimplementedApproved = approvedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedApproved.nonEmpty) {
        log.warn(s"UNIMPLEMENTED ${displayFeatures(unimplementedApproved)} APPROVED ON BLOCKCHAIN")
        log.warn("PLEASE, UPDATE THE NODE AS SOON AS POSSIBLE")
        log.warn("OTHERWISE THE NODE WILL BE STOPPED OR FORKED UPON FEATURE ACTIVATION")
      }

      val activatedFeatures: Set[Short] = blockchain.activatedFeaturesAt(height)

      val unimplementedActivated = activatedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedActivated.nonEmpty) {
        log.error(s"UNIMPLEMENTED ${displayFeatures(unimplementedActivated)} ACTIVATED ON BLOCKCHAIN")
        log.error("PLEASE, UPDATE THE NODE IMMEDIATELY")
        if (settings.featuresSettings.autoShutdownOnUnsupportedFeature) {
          log.error("FOR THIS REASON THE NODE WAS STOPPED AUTOMATICALLY")
          forceStopApplication(UnsupportedFeature)
        } else log.error("OTHERWISE THE NODE WILL END UP ON A FORK")
      }

      approvedFeatures
    } else {

      Set.empty
    }
  }

  override def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]] = {
    val height                             = blockchain.height
    val notImplementedFeatures: Set[Short] = blockchain.activatedFeaturesAt(height).diff(BlockchainFeatures.implemented)

    Either
      .cond(
        !settings.featuresSettings.autoShutdownOnUnsupportedFeature || notImplementedFeatures.isEmpty,
        (),
        GenericError(s"UNIMPLEMENTED ${displayFeatures(notImplementedFeatures)} ACTIVATED ON BLOCKCHAIN, UPDATE THE NODE IMMEDIATELY")
      )
      .flatMap(_ =>
        (ngState match {
          case None =>
            blockchain.lastBlockId match {
              case Some(uniqueId) if uniqueId != block.reference =>
                val logDetails = s"The referenced block(${block.reference})" +
                  s" ${if (blockchain.contains(block.reference)) "exits, it's not last persisted" else "doesn't exist"}"
                Left(BlockAppendError(s"References incorrect or non-existing block: " + logDetails, block))
              case lastBlockId =>
                val height            = lastBlockId.fold(0)(blockchain.unsafeHeightOf)
                val miningConstraints = MiningConstraints(settings.minerSettings, blockchain, height)
                BlockDiffer
                  .fromBlock(functionalitySettings, blockchain, blockchain.lastBlock, block, miningConstraints.total)
                  .map(r => Some((r, Seq.empty[Transaction])))
            }
          case Some(ng) =>
            if (ng.base.reference == block.reference) {
              if (block.blockScore() > ng.base.blockScore()) {
                val height            = blockchain.unsafeHeightOf(ng.base.reference)
                val miningConstraints = MiningConstraints(settings.minerSettings, blockchain, height)

                BlockDiffer
                  .fromBlock(functionalitySettings, blockchain, blockchain.lastBlock, block, miningConstraints.total)
                  .map { r =>
                    log.trace(
                      s"Better liquid block(score=${block.blockScore()}) received and applied instead of existing(score=${ng.base.blockScore()})")
                    Some((r, ng.transactions))
                  }
              } else if (areVersionsOfSameBlock(block, ng.base)) {
                if (block.transactionData.lengthCompare(ng.transactions.size) <= 0) {
                  log.trace(s"Existing liquid block is better than new one, discarding $block")
                  Right(None)
                } else {
                  log.trace(s"New liquid block is better version of existing, swapping")
                  val height            = blockchain.unsafeHeightOf(ng.base.reference)
                  val miningConstraints = MiningConstraints(settings.minerSettings, blockchain, height)

                  BlockDiffer
                    .fromBlock(functionalitySettings, blockchain, blockchain.lastBlock, block, miningConstraints.total)
                    .map(r => Some((r, Seq.empty[Transaction])))
                }
              } else
                Left(BlockAppendError(
                  s"Competitors liquid block $block(score=${block.blockScore()}) is not better than existing (ng.base ${ng.base}(score=${ng.base.blockScore()}))",
                  block))
            } else
              measureSuccessful(forgeBlockTimeStats, ng.totalDiffOf(block.reference)) match {
                case None => Left(BlockAppendError(s"References incorrect or non-existing block", block))
                case Some((referencedForgedBlock, referencedLiquidDiff, carry, discarded)) =>
                  if (referencedForgedBlock.signaturesValid().isRight) {
                    if (discarded.nonEmpty) {
                      microBlockForkStats.increment()
                      microBlockForkHeightStats.record(discarded.size)
                    }

                    val constraint: MiningConstraint = {
                      val height            = blockchain.heightOf(referencedForgedBlock.reference).getOrElse(0)
                      val miningConstraints = MiningConstraints(settings.minerSettings, blockchain, height)
                      miningConstraints.total
                    }

                    val expiredTransactions = blockchain.forgetTransactions((_, txTs) => block.timestamp - txTs > 2 * 60 * 60 * 1000)

                    val diff = BlockDiffer
                      .fromBlock(
                        functionalitySettings,
                        CompositeBlockchain.composite(blockchain, referencedLiquidDiff, carry),
                        Some(referencedForgedBlock),
                        block,
                        constraint
                      )

                    diff.left.foreach { _ =>
                      log.trace(s"Could not append new block, remembering ${expiredTransactions.size} transaction(s)")
                      blockchain.learnTransactions(expiredTransactions)
                    }

                    diff.map { hardenedDiff =>
                      blockchain.append(referencedLiquidDiff, carry, referencedForgedBlock)
                      TxsInBlockchainStats.record(ng.transactions.size)
                      Some((hardenedDiff, discarded.flatMap(_.transactionData)))
                    }
                  } else {
                    val errorText = s"Forged block has invalid signature: base: ${ng.base}, requested reference: ${block.reference}"
                    log.error(errorText)
                    Left(BlockAppendError(errorText, block))
                  }
              }
        }).map {
          _ map {
            case ((newBlockDiff, carry, updatedTotalConstraint), discarded) =>
              val height = blockchain.height + 1
              restTotalConstraint = updatedTotalConstraint
              ngState = Some(new NgState(block, newBlockDiff, carry, featuresApprovedWithBlock(block)))
              lastBlockId.foreach(id => internalLastBlockInfo.onNext(LastBlockInfo(id, height, score, blockchainReady)))
              if ((block.timestamp > time.getTimestamp() - settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis) || (height % 100 == 0)) {
                log.info(s"New height: $height")
              }
              discarded
          }
      })
  }

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[Block]] = {
    val ng = ngState
    if (ng.exists(_.contains(blockId))) {
      log.trace("Resetting liquid block, no rollback is necessary")
      Right(Seq.empty)
    } else {
      val discardedNgBlock = ng.map(_.bestLiquidBlock).toSeq
      ngState = None
      Right(blockchain.rollbackTo(blockId) ++ discardedNgBlock)
    }
  }

  override def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = {
    ngState match {
      case None =>
        Left(MicroBlockAppendError("No base block exists", microBlock))
      case Some(ng) if ng.base.signerData.generator.toAddress != microBlock.sender.toAddress =>
        Left(MicroBlockAppendError("Base block has been generated by another account", microBlock))
      case Some(ng) =>
        ng.lastMicroBlock match {
          case None if ng.base.uniqueId != microBlock.prevResBlockSig =>
            blockMicroForkStats.increment()
            Left(MicroBlockAppendError("It's first micro and it doesn't reference base block(which exists)", microBlock))
          case Some(prevMicro) if prevMicro.totalResBlockSig != microBlock.prevResBlockSig =>
            microMicroForkStats.increment()
            Left(MicroBlockAppendError("It doesn't reference last known microBlock(which exists)", microBlock))
          case _ =>
            for {
              _ <- microBlock.signaturesValid()
              r <- {
                val constraints  = MiningConstraints(settings.minerSettings, blockchain, blockchain.height)
                val mdConstraint = MultiDimensionalMiningConstraint(restTotalConstraint, constraints.micro)
                BlockDiffer.fromMicroBlock(functionalitySettings,
                                           this,
                                           blockchain.lastBlockTimestamp,
                                           microBlock,
                                           ng.base.timestamp,
                                           mdConstraint //,
                                           // verify
                )
              }
            } yield {
              val (diff, carry, updatedMdConstraint) = r
              restTotalConstraint = updatedMdConstraint.constraints.head
              ng.append(microBlock, diff, carry, System.currentTimeMillis)
              log.info(s"$microBlock appended")
              internalLastBlockInfo.onNext(LastBlockInfo(microBlock.totalResBlockSig, height, score, ready = true))
            }
        }
    }
  }

  def shutdown(): Unit = {
    internalLastBlockInfo.onComplete()
    service.shutdown()
  }

  private def newlyApprovedFeatures = ngState.fold(Map.empty[Short, Int])(_.approvedFeatures.map(_ -> height).toMap)

  override def approvedFeatures: Map[Short, Int] = newlyApprovedFeatures ++ blockchain.approvedFeatures

  override def activatedFeatures: Map[Short, Int] =
    newlyApprovedFeatures.mapValues(_ + functionalitySettings.activationWindowSize(height)) ++ blockchain.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = {
    val innerVotes = blockchain.featureVotes(height)
    ngState match {
      case Some(ng) if this.height <= height =>
        val ngVotes = ng.base.featureVotes.map { featureId =>
          featureId -> (innerVotes.getOrElse(featureId, 0) + 1)
        }.toMap

        innerVotes ++ ngVotes
      case _ => innerVotes
    }
  }

  override def feePrice(height: Int): Long = blockchain.feePrice(height)

  override def feeVotes(height: Int): Int = {
    val innerVotes = blockchain.feeVotes(height)
    ngState match {
      case Some(ng) if this.height <= height => innerVotes + ng.base.feeVote
      case _ => innerVotes
    }
  }

  private def liquidBlockHeaderAndSize() = ngState.map { s =>
    (s.bestLiquidBlock, s.bestLiquidBlock.bytes().length)
  }

  override def blockHeaderAndSize(blockId: BlockId): Option[(BlockHeader, Int)] =
    liquidBlockHeaderAndSize().filter(_._1.uniqueId == blockId) orElse blockchain.blockHeaderAndSize(blockId)

  override def height: Int = blockchain.height + ngState.fold(0)(_ => 1)

  override def blockBytes(height: Int): Option[Array[Byte]] =
    blockchain
      .blockBytes(height)
      .orElse(ngState.collect { case ng if height == blockchain.height + 1 => ng.bestLiquidBlock.bytes() })

  override def scoreOf(blockId: BlockId): Option[BigInt] =
    blockchain
      .scoreOf(blockId)
      .orElse(ngState.collect { case ng if ng.contains(blockId) => blockchain.score + ng.base.blockScore() })

  override def blockIdAtHeight(height: Int): Option[BlockId] =
    if (height == this.height)
      ngState.map(_.bestLiquidBlockId)
    else
      blockchain.blockIdAtHeight(height)

  override def heightOf(blockId: BlockId): Option[Int] =
    blockchain
      .heightOf(blockId)
      .orElse(ngState.collect { case ng if ng.contains(blockId) => this.height })

  override def lastBlockIds(howMany: Int): Seq[BlockId] =
    ngState.fold(blockchain.lastBlockIds(howMany))(_.bestLiquidBlockId +: blockchain.lastBlockIds(howMany - 1))

  override def microBlock(id: BlockId): Option[MicroBlock] =
    for {
      ng <- ngState
      mb <- ng.microBlock(id)
    } yield mb

  def lastBlockTimestamp: Option[Long] = ngState.map(_.base.timestamp).orElse(blockchain.lastBlockTimestamp)

  def lastBlockId: Option[AssetId] = ngState.map(_.bestLiquidBlockId).orElse(blockchain.lastBlockId)

  def blockAt(height: Int): Option[Block] =
    if (height == this.height)
      ngState.map(_.bestLiquidBlock)
    else
      blockchain.blockAt(height)

  override def lastPersistedBlockIds(count: Int): Seq[BlockId] = {
    blockchain.lastBlockIds(count)
  }

  override def microblockIds: Seq[BlockId] = ngState.fold(Seq.empty[BlockId])(_.microBlockIds)

  override def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo] = {
    ngState
      .map(_.bestLastBlockInfo(maxTimestamp))
      .orElse(blockchain.lastBlock.map(b => BlockMinerInfo(b.consensusData, b.timestamp, b.uniqueId)))
  }

  override def score: BigInt = blockchain.score + ngState.fold(BigInt(0))(_.bestLiquidBlock.blockScore())

  override def lastBlock: Option[Block] = ngState.map(_.bestLiquidBlock).orElse(blockchain.lastBlock)

  override def carryFee: Long = ngState.map(_.carryFee).getOrElse(blockchain.carryFee)

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] =
    (for {
      ng               <- ngState
      (block, _, _, _) <- ng.totalDiffOf(blockId)
    } yield block.bytes()).orElse(blockchain.blockBytes(blockId))

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = {
    ngState match {
      case Some(ng) if ng.contains(parentSignature) => Some(Seq.empty[ByteStr])
      case maybeNg =>
        blockchain.blockIdsAfter(parentSignature, howMany).map { ib =>
          if (ib.lengthCompare(howMany) < 0) ib ++ maybeNg.map(_.bestLiquidBlockId) else ib
        }
    }
  }

  override def parent(block: Block, back: Int): Option[Block] = {
    ngState match {
      case Some(ng) if ng.contains(block.reference) =>
        if (back == 1) Some(ng.base) else blockchain.parent(ng.base, back - 1)
      case _ =>
        blockchain.parent(block, back)
    }
  }

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] =
    if (height == blockchain.height + 1)
      ngState.map(x => (x.bestLiquidBlock, x.bestLiquidBlock.bytes().length))
    else
      blockchain.blockHeaderAndSize(height)

  override def burned: Long =
    ngState.fold(0L)(_.bestLiquidDiff.burned) + blockchain.burned
  override def burned(height: Int): Long =
    if (height == blockchain.height + 1)
      burned
    else
      blockchain.burned(height)

  override def portfolio(a: Address): Portfolio = {
    val p = ngState.fold(Portfolio.empty)(_.bestLiquidDiff.portfolios.getOrElse(a, Portfolio.empty))
    blockchain.portfolio(a).combine(p)
  }

  private[this] def portfolioAt(a: Address, mb: ByteStr): Portfolio = {
    val p = ngState.fold(Portfolio.empty)(_.diffFor(mb)._1.portfolios.getOrElse(a, Portfolio.empty))
    blockchain.portfolio(a).combine(p)
  }

  override def transactionInfo(id: AssetId): Option[(Int, Transaction)] =
    ngState
      .fold(Diff.empty)(_.bestLiquidDiff)
      .transactions
      .get(id)
      .map(t => (t._1, t._2))
      .orElse(blockchain.transactionInfo(id))

  override def transactionSponsor(id: AssetId): Option[Address] =
    ngState
      .fold(Diff.empty)(_.bestLiquidDiff)
      .feeSponsors
      .get(id)
      .orElse(blockchain.transactionSponsor(id))

  override def addressTransactions(address: Address, types: Set[Byte], count: Int, from: Int): Seq[(Int, Transaction)] = {
    def onlyTypes: Set[Byte] = if (settings.indexAllTransactions) types else if (types.isEmpty) portfolioTxTypes else types.intersect(portfolioTxTypes)

    ngState.fold(blockchain.addressTransactions(address, types, count, from)) { ng =>
      val transactionsFromDiff = ng.bestLiquidDiff.transactions.values.view
        .collect {
          case (height, tx, addresses) if addresses(address) && (onlyTypes.isEmpty || onlyTypes.contains(tx.builder.typeId)) => (height, tx)
        }
        .toSeq

      val diffTxCount = transactionsFromDiff.length

      if (diffTxCount >= from + count)
        transactionsFromDiff.slice(from, from + count)
      else
        transactionsFromDiff.slice(from, from + count) ++ blockchain.addressTransactions(address, types, count - diffTxCount, from - diffTxCount)
    }
  }

  override def containsTransaction(id: AssetId): Boolean = ngState.fold(blockchain.containsTransaction(id)) { ng =>
    ng.bestLiquidDiff.transactions.contains(id) || blockchain.containsTransaction(id)
  }

  override def forgetTransactions(pred: (AssetId, Long) => Boolean): Map[AssetId, Long] = blockchain.forgetTransactions(pred)

  override def learnTransactions(values: Map[AssetId, Long]): Unit = blockchain.learnTransactions(values)

  override def leaseDetails(leaseId: AssetId): Option[LeaseDetails] = ngState match {
    case Some(ng) =>
      blockchain.leaseDetails(leaseId).map(ld => ld.copy(isActive = ng.bestLiquidDiff.leaseState.getOrElse(leaseId, ld.isActive))) orElse
        ng.bestLiquidDiff.transactions.get(leaseId).collect {
          case (h, lt: LeaseTransaction, _) =>
            LeaseDetails(lt.sender, lt.recipient, h, lt.amount, ng.bestLiquidDiff.leaseState(lt.id()))
        }
    case None =>
      blockchain.leaseDetails(leaseId)
  }

  /** Retrieves LTO balance snapshot in the [from, to] range (inclusive) */
  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = {
    val blockchainBlock = blockchain.heightOf(to)
    if (blockchainBlock.nonEmpty || ngState.isEmpty) {
      blockchain.balanceSnapshots(address, from, to)
    } else {
      val bs = BalanceSnapshot(height, portfolioAt(address, to))
      if (blockchain.height > 0 && from < this.height) bs +: blockchain.balanceSnapshots(address, from, to) else Seq(bs)
    }
  }

  override def accountScript(address: Address): Option[Script] = ngState.fold(blockchain.accountScript(address)) { ng =>
    ng.bestLiquidDiff.scripts.get(address) match {
      case None            => blockchain.accountScript(address)
      case Some(None)      => None
      case Some(Some(scr)) => Some(scr)
    }
  }

  override def hasScript(address: Address): Boolean = ngState.fold(blockchain.hasScript(address)) { ng =>
    ng.bestLiquidDiff.scripts.contains(address) || blockchain.hasScript(address)
  }

  override def accountData(acc: Address): AccountDataInfo = ngState.fold(blockchain.accountData(acc)) { ng =>
    val fromInner = blockchain.accountData(acc)
    val fromDiff  = ng.bestLiquidDiff.accountData.get(acc).orEmpty
    fromInner.combine(fromDiff)
  }

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = ngState.fold(blockchain.accountData(acc, key)) { ng =>
    val diffData = ng.bestLiquidDiff.accountData.get(acc).orEmpty
    diffData.data.get(key).orElse(blockchain.accountData(acc, key))
  }

  override def certificate(acc: Address): Option[Array[Byte]] = ngState.fold(blockchain.certificate(acc)) { ng =>
    ng.bestLiquidDiff.certificate.get(acc).flatten.orElse(blockchain.certificate(acc))
  }

  private def changedBalances(pred: Portfolio => Boolean, f: Address => Long): Map[Address, Long] =
    ngState
      .fold(Map.empty[Address, Long]) { ng =>
        for {
          (address, p) <- ng.bestLiquidDiff.portfolios
          if pred(p)
        } yield address -> f(address)
      }

  override def ltoDistribution(height: Int): Map[Address, Long] = ngState.fold(blockchain.ltoDistribution(height)) { ng =>
    val innerDistribution = blockchain.ltoDistribution(height)
    if (height < this.height) innerDistribution
    else {
      innerDistribution ++ changedBalances(_.balance != 0, portfolio(_).balance)
    }
  }

  override def sponsorOf(address: Address): List[Address] = ngState.fold(blockchain.sponsorOf(address)) { ng =>
    ng.bestLiquidDiff.sponsoredBy.get(address) match {
      case Some(l) => l
      case None    => blockchain.sponsorOf(address)
    }
  }

  override def allActiveLeases: Set[LeaseTransaction] = ngState.fold(blockchain.allActiveLeases) { ng =>
    val (active, canceled) = ng.bestLiquidDiff.leaseState.partition(_._2)
    val fromDiff = active.keys
      .map { id =>
        ng.bestLiquidDiff.transactions(id)._2
      }
      .collect { case lt: LeaseTransaction => lt }
      .toSet
    val fromInner = blockchain.allActiveLeases.filterNot(ltx => canceled.keySet.contains(ltx.id()))
    fromDiff ++ fromInner
  }

  /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
    *
    * @note Portfolios passed to `pf` only contain LTO and Leasing balances to improve performance */
  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] =
    ngState.fold(blockchain.collectLposPortfolios(pf)) { ng =>
      val b = Map.newBuilder[Address, A]
      for ((a, p) <- ng.bestLiquidDiff.portfolios if p.lease != LeaseBalance.empty || p.balance != 0) {
        pf.runWith(b += a -> _)(a -> portfolio(a).copy())
      }

      blockchain.collectLposPortfolios(pf) ++ b.result()
    }

  override def append(diff: Diff, carry: Long, block: Block): Unit = blockchain.append(diff, carry, block)

  override def rollbackTo(targetBlockId: AssetId): Seq[Block] = blockchain.rollbackTo(targetBlockId)

  override def transactionHeight(id: AssetId): Option[Int] = ngState match {
    case Some(ng) => ng.bestLiquidDiff.transactions.get(id).map(_._1)
    case None     => blockchain.transactionHeight(id)
  }

  override def balance(address: Address): Long = ngState match {
    case Some(ng) =>
      blockchain.balance(address) + ng.bestLiquidDiff.portfolios.getOrElse(address, Portfolio.empty).balance
    case None =>
      blockchain.balance(address)
  }

  override def associations(address: Address): Associations = {
    val associations = blockchain.associations(address)
    val timestamp = blockchain.lastBlockTimestamp.get + 1

    val txs = ngState
      .map(
        _.bestLiquidDiff.transactions.values
          .filter { case (_, tx, _) =>
            tx.builder.typeId == IssueAssociationTransaction.typeId || tx.builder.typeId == RevokeAssociationTransaction.typeId
          }
          .map { case (_, tx, _) => (timestamp, tx.asInstanceOf[AssociationTransaction]) }
          .toList
      )
      .getOrElse(List.empty)

    associations.update(timestamp, txs)
  }
}

object BlockchainUpdaterImpl extends ScorexLogging {

  import kamon.metric.instrument.{Time => KTime}

  private val blockMicroForkStats       = Kamon.metrics.counter("block-micro-fork")
  private val microMicroForkStats       = Kamon.metrics.counter("micro-micro-fork")
  private val microBlockForkStats       = Kamon.metrics.counter("micro-block-fork")
  private val microBlockForkHeightStats = Kamon.metrics.histogram("micro-block-fork-height")
  private val forgeBlockTimeStats       = Kamon.metrics.histogram("forge-block-time", KTime.Milliseconds)

  def areVersionsOfSameBlock(b1: Block, b2: Block): Boolean =
    b1.signerData.generator == b2.signerData.generator &&
      b1.consensusData.baseTarget == b2.consensusData.baseTarget &&
      b1.reference == b2.reference &&
      b1.timestamp == b2.timestamp
}
