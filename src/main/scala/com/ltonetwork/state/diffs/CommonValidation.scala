package com.ltonetwork.state.diffs

import cats._
import com.ltonetwork.account.Address
import com.ltonetwork.features.FeatureProvider._
import com.ltonetwork.features.{BlockchainFeature, BlockchainFeatures}
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError._
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.association.AssociationTransaction
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease._
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.sponsorship._
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
        NotActivated(s"Version ${tx.version} of ${tx.getClass.getSimpleName} (tx type ${tx.typeId}) has not been activated yet")
      )
    def deactivationBarrier(b: BlockchainFeature) =
      Either.cond(
        !blockchain.isFeatureActivated(b, height),
        tx,
        NotActivated(s"Version ${tx.version} of ${tx.getClass.getSimpleName} (tx type ${tx.typeId}) has been deactivated")
      )

    (tx, tx.version) match {
      case (_: GenesisTransaction, 1)          => Right(tx)
      case (_: TransferTransaction, 1)         => Right(tx)
      case (_: TransferTransaction, 2)         => activationBarrier(BlockchainFeatures.SmartAccounts)
      case (_: TransferTransaction, 3)         => activationBarrier(BlockchainFeatures.TransactionsV3)
      case (_: LeaseTransaction, 1)            => Right(tx)
      case (_: LeaseTransaction, 2)            => activationBarrier(BlockchainFeatures.SmartAccounts)
      case (_: LeaseTransaction, 3)            => activationBarrier(BlockchainFeatures.TransactionsV3)
      case (_: CancelLeaseTransaction, 1)      => Right(tx)
      case (_: CancelLeaseTransaction, 2)      => activationBarrier(BlockchainFeatures.SmartAccounts)
      case (_: CancelLeaseTransaction, 3)      => activationBarrier(BlockchainFeatures.TransactionsV3)
      case (_: MassTransferTransaction, 1)     => Right(tx)
      case (_: MassTransferTransaction, 3)     => activationBarrier(BlockchainFeatures.TransactionsV3)
      case (_: DataTransaction, 1)             => deactivationBarrier(BlockchainFeatures.SmartAccounts)
      case (_: SetScriptTransaction, 1)        => Right(tx)
      case (_: SetScriptTransaction, 3)        => activationBarrier(BlockchainFeatures.TransactionsV3)
      case (_: AnchorTransaction, 1)           => Right(tx)
      case (_: AnchorTransaction, 3)           => activationBarrier(BlockchainFeatures.TransactionsV3)
      case (_: AssociationTransaction, 1)      => activationBarrier(BlockchainFeatures.AssociationTransaction)
      case (_: AssociationTransaction, 3)      => activationBarrier(BlockchainFeatures.TransactionsV3)
      case (_: SponsorshipTransactionBase, 1)  => activationBarrier(BlockchainFeatures.SponsorshipTransaction)
      case (_: SponsorshipTransactionBase, 3)  => activationBarrier(BlockchainFeatures.TransactionsV3)

      case _                                   => Left(ActivationError(s"Version ${tx.version} of ${tx.getClass.getSimpleName} (tx type ${tx.typeId}) must be explicitly activated"))
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

  private def feeInUnitsVersion1(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction       => Right(0)
    case _: TransferTransaction      => Right(1)
    case tx: MassTransferTransaction => Right(1 + (tx.transfers.size + 1) / 2)
    case _: LeaseTransaction         => Right(1)
    case _: CancelLeaseTransaction   => Right(1)
    case tx: DataTransaction         => Right(1 + (tx.bodyBytes().length - 1) / 1024)
    case tx: AnchorTransaction       => Right(1 + (tx.bodyBytes().length - 1) / 1024)
    case _: SetScriptTransaction     => Right(1)
    case _                           => Left(UnsupportedTransactionType)
  }

  private def feeInUnitsVersion2(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction         => Right(0)
    case _: TransferTransaction        => Right(1000)
    case _: LeaseTransaction           => Right(1000)
    case _: SetScriptTransaction       => Right(1000)
    case _: CancelLeaseTransaction     => Right(1000)
    case tx: MassTransferTransaction   => Right(1000 + tx.transfers.size * 100)
    case _: AnchorTransaction          => Right(100)
    case _: AssociationTransaction     => Right(1000)
    case _: SponsorshipTransactionBase => Right(5000)
    case _                             => Left(UnsupportedTransactionType)
  }

  private def feeInUnitsVersion3(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction         => Right(0)
    case _: TransferTransaction        => Right(1000)
    case _: LeaseTransaction           => Right(1000)
    case _: SetScriptTransaction       => Right(1000)
    case _: CancelLeaseTransaction     => Right(1000)
    case tx: MassTransferTransaction   => Right(1000 + tx.transfers.size * 100)
    case _: AnchorTransaction          => Right(350)
    case _: AssociationTransaction     => Right(1000)
    case _: SponsorshipTransactionBase => Right(5000)
    case _                             => Left(UnsupportedTransactionType)
  }

  private def feeInUnitsVersion4(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction           => Right(0)
    case _: TransferTransaction          => Right(10)
    case _: LeaseTransaction             => Right(10)
    case _: SetScriptTransaction         => Right(100)
    case _: CancelLeaseTransaction       => Right(10)
    case tx: MassTransferTransaction     => Right(10 + tx.transfers.size * 1)
    case tx: AnchorTransaction           => Right(10 + tx.anchors.size * 1)
    case _: AssociationTransaction       => Right(10)
    case _: SponsorshipTransaction       => Right(100)
    case _: CancelSponsorshipTransaction => Right(10)
    case _                               => Left(UnsupportedTransactionType)
  }

  def getMinFee(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: Transaction): Either[ValidationError, Long] = {
    if (blockchain.isFeatureActivated(BlockchainFeatures.TransactionsV3, height))
      feeInUnitsVersion4(tx)
    else if (blockchain.isFeatureActivated(BlockchainFeatures.BurnFeeture, height))
      feeInUnitsVersion3(tx)
    else if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts, height))
      feeInUnitsVersion2(tx)
    else
      feeInUnitsVersion1(tx)
  }.map(_ * Sponsorship.FeeUnit)

  def checkFee(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: Transaction): Either[ValidationError, Unit] =
    getMinFee(blockchain, fs, height, tx)
      .flatMap(minFee => Either.cond(tx.fee >= minFee, (), InsufficientFee(s"Not enough fee, actual: ${tx.fee} required: $minFee")))
}
