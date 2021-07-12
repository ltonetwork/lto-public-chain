package com.ltonetwork.state.diffs

import cats._
import com.ltonetwork.account.Address
import com.ltonetwork.features.FeatureProvider._
import com.ltonetwork.features.{BlockchainFeature, BlockchainFeatures}
import com.ltonetwork.settings.{Constants, FunctionalitySettings}
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError._
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease._
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.sponsorship.SponsorshipTransactionBase
import com.ltonetwork.transaction.transfer._

import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration     = 90.minutes
  val MaxTimePrevBlockOverTransactionDiff: FiniteDuration = 2.hours
  val ScriptExtraFee                                      = 100000000L

  def disallowSendingGreaterThanBalance[T <: Transaction](blockchain: Blockchain,
                                                          settings: FunctionalitySettings,
                                                          blockTime: Long,
                                                          tx: T): Either[ValidationError, T] = {
    def checkTransfer(sender: Address, amount: Long, feeAmount: Long) = {
      val amountDiff = Portfolio(-amount)

      val feeDiff = Portfolio(-feeAmount)

      val spendings     = Monoid.combine(amountDiff, feeDiff)
      val oldLtoBalance = blockchain.portfolio(sender).balance

      val newLtoBalance = oldLtoBalance + spendings.balance
      if (newLtoBalance < 0) {
        Left(
          GenericError(
            "Attempt to transfer unavailable funds: Transaction application leads to " +
              s"negative lto balance to (at least) temporary negative state, current balance equals $oldLtoBalance, " +
              s"spends equals ${spendings.balance}, result is $newLtoBalance"))
      } else Right(tx)
    }

    tx match {
      case ttx: TransferTransaction     => checkTransfer(ttx.sender, ttx.amount, ttx.fee)
      case mtx: MassTransferTransaction => checkTransfer(mtx.sender, mtx.transfers.map(_.amount).sum, mtx.fee)
      case _                            => Right(tx)
    }
  }
  def disallowDuplicateIds[T <: Transaction](blockchain: Blockchain,
                                             settings: FunctionalitySettings,
                                             height: Int,
                                             tx: T): Either[ValidationError, T] = tx match {
    case _ => if (blockchain.containsTransaction(tx.id())) Left(AlreadyInTheState(tx.id(), 0)) else Right(tx)
  }

  def disallowBeforeActivationTime[T <: Transaction](blockchain: Blockchain, height: Int, tx: T): Either[ValidationError, T] = {

    def activationBarrier(b: BlockchainFeature) =
      Either.cond(
        blockchain.isFeatureActivated(b, height),
        tx,
        NotActivated(s"${tx.getClass.getSimpleName} transaction has not been activated yet")
      )
    def deactivationBarrier(b: BlockchainFeature) =
      Either.cond(
        !blockchain.isFeatureActivated(b, height),
        tx,
        NotActivated(s"${tx.getClass.getSimpleName} transaction has been deactivated")
      )

    (tx, tx.version) match {
      case (_: GenesisTransaction, 1)          => Right(tx)
      case (_: TransferTransaction, 1)         => Right(tx)
      case (_: TransferTransaction, 2)         => activationBarrier(BlockchainFeatures.SmartAccounts)
      case (_: LeaseTransaction, 1)            => Right(tx)
      case (_: LeaseTransaction, 2)            => activationBarrier(BlockchainFeatures.SmartAccounts)
      case (_: CancelLeaseTransaction, 1)      => Right(tx)
      case (_: CancelLeaseTransaction, 2)      => activationBarrier(BlockchainFeatures.SmartAccounts)
      case (_: MassTransferTransaction, 1)     => Right(tx)
      case (_: DataTransaction, 1)             => deactivationBarrier(BlockchainFeatures.SmartAccounts)
      case (_: SetScriptTransaction, 1)        => Right(tx)
      case (_: AnchorTransaction, 1)           => Right(tx)
      case (_: IssueAssociationTransaction, 1) => activationBarrier(BlockchainFeatures.AssociationTransaction)
      case (_: SponsorshipTransactionBase, 1)  => activationBarrier(BlockchainFeatures.SponsorshipTransaction)

      // Needs to be behind a feature flag
      case (_: AnchorTransaction, 3)           => Right(tx)
      case (_: AssociationTransaction, 3)      => Right(tx)
      case (_: LeaseTransaction, 3)            => Right(tx)
      case (_: CancelLeaseTransaction, 3)      => Right(tx)
      case (_: SetScriptTransaction, 3)        => Right(tx)
      case (_: SponsorshipTransactionBase, 3)  => Right(tx)
      case (_: TransferTransaction, 3)         => Right(tx)
      case (_: MassTransferTransaction, 3)     => Right(tx)
      case _                                   => Left(ActivationError(s"Version ${tx.version} of transaction type ${tx.typeId} must be explicitly activated"))
    }
  }

  def disallowTxFromFuture[T <: Transaction](settings: FunctionalitySettings, time: Long, tx: T): Either[ValidationError, T] = {
    if (tx.timestamp - time > MaxTimeTransactionOverBlockDiff.toMillis)
      Left(Mistiming(s"Transaction ts ${tx.timestamp} is from far future. BlockTime: $time"))
    else Right(tx)
  }

  def disallowTxFromPast[T <: Transaction](prevBlockTime: Option[Long], tx: T): Either[ValidationError, T] =
    prevBlockTime match {
      case Some(t) if (t - tx.timestamp) > MaxTimePrevBlockOverTransactionDiff.toMillis =>
        Left(Mistiming(s"Transaction ts ${tx.timestamp} is too old. Previous block time: $prevBlockTime"))
      case _ => Right(tx)
    }

  private def oldFeeInUnits(blockchain: Blockchain, height: Int, tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction       => Right(0)
    case _: TransferTransaction      => Right(1)
    case tx: MassTransferTransaction => Right(1 + (tx.transfers.size + 1) / 2)
    case _: LeaseTransaction         => Right(1)
    case _: CancelLeaseTransaction   => Right(1)
    case tx: DataTransaction =>
      val base = if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts, height)) tx.bodyBytes() else tx.bytes()
      Right(1 + (base.length - 1) / 1024)
    case tx: AnchorTransaction   => Right(1 + (tx.bodyBytes().length - 1) / 1024)
    case _: SetScriptTransaction => Right(1)
    case _                       => Left(UnsupportedTransactionType)
  }

  private def newFeeInUnits(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction          => Right(0)
    case _: TransferTransaction         => Right(1000)
    case _: LeaseTransaction            => Right(1000)
    case _: SetScriptTransaction        => Right(1000)
    case _: CancelLeaseTransaction      => Right(1000)
    case tx: MassTransferTransaction    => Right(1000 + tx.transfers.size * 100)
    case _: AnchorTransaction           => Right(100)
    case _: IssueAssociationTransaction => Right(1000)
    case _: SponsorshipTransactionBase  => Right(5000)
    case _                              => Left(UnsupportedTransactionType)
  }

  private def superNewFeeInUnits(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction          => Right(0)
    case _: TransferTransaction         => Right(1000)
    case _: LeaseTransaction            => Right(1000)
    case _: SetScriptTransaction        => Right(1000)
    case _: CancelLeaseTransaction      => Right(1000)
    case tx: MassTransferTransaction    => Right(1000 + tx.transfers.size * 100)
    case _: AnchorTransaction           => Right(350)
    case _: IssueAssociationTransaction => Right(1000)
    case _: SponsorshipTransactionBase  => Right(5000)
    case _                              => Left(UnsupportedTransactionType)
  }

  def getMinFee(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: Transaction): Either[ValidationError, Long] = {

    def oldFees() = {
      type FeeInfo = Long

      def hasSmartAccountScript: Boolean = tx match {
        case _: GenesisTransaction => false
        case _                     => blockchain.hasScript(tx.sender)
      }

      def feeAfterSmartAccounts(inputFee: FeeInfo): FeeInfo =
        if (hasSmartAccountScript) {
          inputFee + ScriptExtraFee
        } else inputFee

      oldFeeInUnits(blockchain, height, tx)
        .map(_ * Sponsorship.FeeUnit)
        .map(feeAfterSmartAccounts)
    }
    def newFees()      = newFeeInUnits(tx).map(_ * Sponsorship.FeeUnit)
    def superNewFees() = superNewFeeInUnits(tx).map(_ * Sponsorship.FeeUnit)
    if (blockchain.isFeatureActivated(BlockchainFeatures.BurnFeeture, height))
      superNewFees()
    else if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts, height))
      newFees()
    else oldFees()
  }

  def checkFee(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: Transaction): Either[ValidationError, Unit] = {
    def oldFees() = {
      def restFee(inputFee: Long): Either[ValidationError, (Option[AssetId], Long)] = {
        val txName    = Constants.TransactionNames(tx.typeId)
        val feeAmount = inputFee
        for {
          feeInUnits <- oldFeeInUnits(blockchain, height, tx)
          minimumFee    = feeInUnits * Sponsorship.FeeUnit
          restFeeAmount = feeAmount - minimumFee
          _ <- Either.cond(
            restFeeAmount >= 0,
            (),
            GenericError(s"Fee in LTO for ${txName} does not exceed minimal value of $minimumFee LTOs: $feeAmount")
          )
        } yield (None, restFeeAmount)
      }

      def hasSmartAccountScript: Boolean = tx match {
        case _: GenesisTransaction => false
        case _                     => blockchain.hasScript(tx.sender)
      }

      def restFeeAfterSmartAccounts(inputFee: (Option[AssetId], Long)): Either[ValidationError, (Option[AssetId], Long)] =
        if (hasSmartAccountScript) {
          val (feeAssetId, feeAmount) = inputFee
          for {
            _ <- Either.cond(feeAssetId.isEmpty, (), GenericError("Transactions from scripted accounts require LTO as fee"))
            _ <- Either.cond(
              feeAmount >= 0,
              (),
              InsufficientFee()
            )
          } yield (feeAssetId, feeAmount)
        } else Right(inputFee)

      restFee(tx.fee)
        .flatMap(restFeeAfterSmartAccounts)
        .map(_ => ())
    }

    def newFees() =
      newFeeInUnits(tx)
        .map(_ * Sponsorship.FeeUnit)
        .flatMap(minFee => Either.cond(tx.fee >= minFee, (), InsufficientFee(s"Not enough fee, actual: ${tx.fee} required: $minFee")))

    if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts, height))
      newFees()
    else oldFees()
  }

}
