package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.account.Address
import com.wavesplatform.transaction.ValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer._

import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration     = 90.minutes
  val MaxTimePrevBlockOverTransactionDiff: FiniteDuration = 2.hours
  val ScriptExtraFee                                      = 400000L

  def disallowSendingGreaterThanBalance[T <: Transaction](blockchain: Blockchain,
                                                          settings: FunctionalitySettings,
                                                          blockTime: Long,
                                                          tx: T): Either[ValidationError, T] =
    if (blockTime >= settings.allowTemporaryNegativeUntil) {
      def checkTransfer(sender: Address, amount: Long, feeAmount: Long) = {
        val amountDiff = Portfolio(-amount, LeaseBalance.empty)

        val feeDiff = Portfolio(-feeAmount, LeaseBalance.empty)

        val spendings       = Monoid.combine(amountDiff, feeDiff)
        val oldWavesBalance = blockchain.portfolio(sender).balance

        val newWavesBalance = oldWavesBalance + spendings.balance
        if (newWavesBalance < 0) {
          Left(
            GenericError(
              "Attempt to transfer unavailable funds: Transaction application leads to " +
                s"negative lto balance to (at least) temporary negative state, current balance equals $oldWavesBalance, " +
                s"spends equals ${spendings.balance}, result is $newWavesBalance"))
        } else Right(tx)
      }

      tx match {
        case ptx: PaymentTransaction if blockchain.portfolio(ptx.sender).balance < (ptx.amount + ptx.fee) =>
          Left(
            GenericError(
              "Attempt to pay unavailable funds: balance " +
                s"${blockchain.portfolio(ptx.sender).balance} is less than ${ptx.amount + ptx.fee}"))
        case ttx: TransferTransaction     => checkTransfer(ttx.sender, ttx.amount, ttx.fee)
        case mtx: MassTransferTransaction => checkTransfer(mtx.sender, mtx.transfers.map(_.amount).sum, mtx.fee)
        case _                            => Right(tx)
      }
    } else Right(tx)

  def disallowDuplicateIds[T <: Transaction](blockchain: Blockchain,
                                             settings: FunctionalitySettings,
                                             height: Int,
                                             tx: T): Either[ValidationError, T] = tx match {
    case _: PaymentTransaction => Right(tx)
    case _                     => if (blockchain.containsTransaction(tx.id())) Left(AlreadyInTheState(tx.id(), 0)) else Right(tx)
  }

  def disallowBeforeActivationTime[T <: Transaction](blockchain: Blockchain, height: Int, tx: T): Either[ValidationError, T] = {

    def activationBarrier(b: BlockchainFeature) =
      Either.cond(
        blockchain.isFeatureActivated(b, height),
        tx,
        ValidationError.ActivationError(s"${tx.getClass.getSimpleName} transaction has not been activated yet")
      )

    val disabled = Left(GenericError("tx type is disabled"))

    tx match {
      case _: BurnTransactionV1        => disabled
      case _: PaymentTransaction       => disabled
      case _: GenesisTransaction       => Right(tx)
      case _: TransferTransactionV1    => Right(tx)
      case _: IssueTransactionV1       => disabled
      case _: ReissueTransactionV1     => disabled
      case _: ExchangeTransaction      => disabled
      case _: LeaseTransactionV1       => Right(tx)
      case _: LeaseCancelTransactionV1 => Right(tx)
      case _: CreateAliasTransactionV1 => disabled
      case _: MassTransferTransaction  => activationBarrier(BlockchainFeatures.MassTransfer)
      case _: DataTransaction          => activationBarrier(BlockchainFeatures.DataTransaction)
      case _: SetScriptTransaction     => disabled
      case _: TransferTransactionV2    => activationBarrier(BlockchainFeatures.SmartAccounts)
      case it: IssueTransactionV2      => disabled
      case _: ReissueTransactionV2     => disabled
      case _: BurnTransactionV2        => disabled
      case _: LeaseTransactionV2       => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: LeaseCancelTransactionV2 => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: CreateAliasTransactionV2 => disabled
      case _: SponsorFeeTransaction    => disabled
      case _: AnchorTransaction        => Right(tx)
      case _                           => Left(GenericError("Unknown transaction must be explicitly activated"))
    }
  }

  def disallowTxFromFuture[T <: Transaction](settings: FunctionalitySettings, time: Long, tx: T): Either[ValidationError, T] = {
    val allowTransactionsFromFutureByTimestamp = tx.timestamp < settings.allowTransactionsFromFutureUntil
    if (!allowTransactionsFromFutureByTimestamp && tx.timestamp - time > MaxTimeTransactionOverBlockDiff.toMillis)
      Left(Mistiming(s"Transaction ts ${tx.timestamp} is from far future. BlockTime: $time"))
    else Right(tx)
  }

  def disallowTxFromPast[T <: Transaction](prevBlockTime: Option[Long], tx: T): Either[ValidationError, T] =
    prevBlockTime match {
      case Some(t) if (t - tx.timestamp) > MaxTimePrevBlockOverTransactionDiff.toMillis =>
        Left(Mistiming(s"Transaction ts ${tx.timestamp} is too old. Previous block time: $prevBlockTime"))
      case _ => Right(tx)
    }

  private def feeInUnits(blockchain: Blockchain, height: Int, tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction       => Right(0)
    case _: PaymentTransaction       => Right(1)
    case _: IssueTransaction         => Right(1000)
    case _: ReissueTransaction       => Right(1000)
    case _: BurnTransaction          => Right(1)
    case _: TransferTransaction      => Right(1)
    case tx: MassTransferTransaction => Right(1 + (tx.transfers.size + 1) / 2)
    case _: LeaseTransaction         => Right(1)
    case _: LeaseCancelTransaction   => Right(1)
    case _: ExchangeTransaction      => Right(3)
    case _: CreateAliasTransaction   => Right(1)
    case tx: DataTransaction =>
      val base = if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts, height)) tx.bodyBytes() else tx.bytes()
      Right(1 + (base.length - 1) / 1024)
    case tx: AnchorTransaction    => Right(1 + (tx.bodyBytes().length - 1) / 1024)
    case _: SetScriptTransaction  => Right(1)
    case _: SponsorFeeTransaction => Right(1000)
    case _                        => Left(UnsupportedTransactionType)
  }

  def getMinFee(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: Transaction): Either[ValidationError, (Option[AssetId], Long)] = {
    type FeeInfo = (Option[(AssetId, AssetDescription)], Long)

    def feeAfterSponsorship(txAsset: Option[AssetId]): Either[ValidationError, FeeInfo] =
      if (height < Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) {
        // This could be true for private blockchains
        feeInUnits(blockchain, height, tx).map(x => (None, x * Sponsorship.FeeUnit))
      } else
        for {
          feeInUnits <- feeInUnits(blockchain, height, tx)
          r          <- Right((None, feeInUnits * Sponsorship.FeeUnit))
        } yield r

    def isSmartToken(input: FeeInfo): Boolean = input._1.map(_._1).flatMap(blockchain.assetDescription).exists(_.script.isDefined)

    def feeAfterSmartTokens(inputFee: FeeInfo): Either[ValidationError, FeeInfo] = Right {
      if (isSmartToken(inputFee)) {
        val (feeAssetInfo, feeAmount) = inputFee
        (feeAssetInfo, feeAmount + ScriptExtraFee)
      } else inputFee
    }

    def hasSmartAccountScript: Boolean = tx match {
      case tx: Transaction with Authorized => blockchain.hasScript(tx.sender)
      case _                               => false
    }

    def feeAfterSmartAccounts(inputFee: FeeInfo): Either[ValidationError, FeeInfo] = Right {
      if (hasSmartAccountScript) {
        val (feeAssetInfo, feeAmount) = inputFee
        (feeAssetInfo, feeAmount + ScriptExtraFee)
      } else inputFee
    }

    feeAfterSponsorship(None)
      .flatMap(feeAfterSmartTokens)
      .flatMap(feeAfterSmartAccounts)
      .map {
        case (Some((assetId, assetInfo)), amountInWaves) => (Some(assetId), Sponsorship.fromWaves(amountInWaves, assetInfo.sponsorship))
        case (None, amountInWaves)                       => (None, amountInWaves)
      }
  }

  def checkFee(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: Transaction): Either[ValidationError, Unit] = {
    def restFeeAfterSponsorship(inputFee: Long): Either[ValidationError, (Option[AssetId], Long)] =
      if (height < Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) Right(None, inputFee)
      else {
        val feeAmount = inputFee
        for {
          feeInUnits <- feeInUnits(blockchain, height, tx)
          feeAmount  <- Right(feeAmount)
          minimumFee    = feeInUnits * Sponsorship.FeeUnit
          restFeeAmount = feeAmount - minimumFee
          _ <- Either.cond(
            restFeeAmount >= 0,
            (),
            GenericError(s"Fee in WAVES for ${tx.builder.classTag} does not exceed minimal value of $minimumFee WAVES: $feeAmount")
          )
        } yield (None, restFeeAmount)
      }

    def isSmartToken: Boolean = false

    def restFeeAfterSmartTokens(inputFee: (Option[AssetId], Long)): Either[ValidationError, (Option[AssetId], Long)] =
      if (isSmartToken) {
        val (feeAssetId, feeAmount) = inputFee
        for {
          _ <- Either.cond(feeAssetId.isEmpty, (), GenericError("Transactions with smart tokens require LTO as fee"))
          restFeeAmount = feeAmount - ScriptExtraFee
          _ <- Either.cond(
            restFeeAmount >= 0,
            (),
            InsufficientFee(s"This transaction with a smart token requires ${-restFeeAmount} additional fee")
          )
        } yield (feeAssetId, restFeeAmount)
      } else Right(inputFee)

    def hasSmartAccountScript: Boolean = tx match {
      case tx: Transaction with Authorized => blockchain.hasScript(tx.sender)
      case _                               => false
    }

    def restFeeAfterSmartAccounts(inputFee: (Option[AssetId], Long)): Either[ValidationError, (Option[AssetId], Long)] =
      if (hasSmartAccountScript) {
        val (feeAssetId, feeAmount) = inputFee
        for {
          _ <- Either.cond(feeAssetId.isEmpty, (), GenericError("Transactions from scripted accounts require Waves as fee"))
          restFeeAmount = feeAmount - ScriptExtraFee
          _ <- Either.cond(
            restFeeAmount >= 0,
            (),
            InsufficientFee(s"Scripted account requires ${-restFeeAmount} additional fee for this transaction")
          )
        } yield (feeAssetId, restFeeAmount)
      } else Right(inputFee)

    restFeeAfterSponsorship(tx.fee)
      .flatMap(restFeeAfterSmartTokens)
      .flatMap(restFeeAfterSmartAccounts)
      .map(_ => ())
  }
}
