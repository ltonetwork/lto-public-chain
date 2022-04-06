package com.ltonetwork.state.diffs

import cats.kernel.Monoid
import com.ltonetwork.account.Address
import com.ltonetwork.fee.FeeCalculator
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.UnsupportedTransactionType
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.anchor.{AnchorTransaction, MappedAnchorTransaction}
import com.ltonetwork.transaction.association.AssociationTransaction
import com.ltonetwork.transaction.burn.BurnTransaction
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer._

object TransactionDiffer {

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError

  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: Transaction): Either[ValidationError, Diff] = {
    for {
      _ <- CommonValidation.disallowUnsupportedKeyTypes(blockchain, currentBlockHeight, tx)
      _ <- Verifier(blockchain, currentBlockHeight)(tx)
      _ <- CommonValidation.disallowTxFromFuture(settings, currentBlockTimestamp, tx)
      _ <- CommonValidation.disallowTxFromPast(prevBlockTimestamp, tx)
      _ <- CommonValidation.disallowBeforeActivationTime(blockchain, currentBlockHeight, tx)
      _ <- CommonValidation.disallowDuplicateIds(blockchain, settings, currentBlockHeight, tx)
      _ <- CommonValidation.disallowSendingGreaterThanBalance(blockchain, settings, currentBlockTimestamp, tx)
      _ <- CommonValidation.checkFee(blockchain, settings, currentBlockHeight, tx)
      diff <- tx match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(currentBlockHeight)(gtx)
        case _ =>
          (tx match {
            case ttx: TransferTransaction     => TransferTransactionDiff(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
            case mtx: MassTransferTransaction => MassTransferTransactionDiff(blockchain, currentBlockTimestamp, currentBlockHeight)(mtx)
            case ltx: LeaseTransaction        => LeaseTransactionsDiff.lease(blockchain, currentBlockHeight)(ltx)
            case ltx: CancelLeaseTransaction =>
              LeaseTransactionsDiff.leaseCancel(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
            case dtx: DataTransaction               => DataTransactionDiff(blockchain, currentBlockHeight)(dtx)
            case sstx: SetScriptTransaction         => SetScriptTransactionDiff(blockchain, currentBlockHeight)(sstx)
            case at: AnchorTransaction              => AnchorTransactionDiff(blockchain, currentBlockHeight)(at)
            case as: AssociationTransaction         => AssociationTransactionDiff(blockchain, currentBlockHeight)(as)
            case stx: SponsorshipTransaction        => SponsorshipTransactionDiff.sponsor(blockchain, currentBlockHeight)(stx)
            case sctx: CancelSponsorshipTransaction => SponsorshipTransactionDiff.cancel(blockchain, currentBlockHeight)(sctx)
            case rtx: RegisterTransaction           => RegisterTransactionDiff(blockchain, currentBlockHeight)(rtx)
            case btx: BurnTransaction               => BurnTransactionDiff(blockchain, currentBlockHeight)(btx)
            case natx: MappedAnchorTransaction       => MappedAnchorTransactionDiff(blockchain, currentBlockHeight)(natx)
            case _                                  => Left(UnsupportedTransactionType)
          }).map { d: Diff =>
            val fee = FeeCalculator(blockchain).fee(currentBlockHeight, tx)

            // Sponsored transaction
            val feeAccount: Address = tx.sponsor.getOrElse(tx.sender).toAddress

            // Sponsored account
            val feePayer: Address = blockchain
              .sponsorOf(feeAccount)
              .find(a => blockchain.portfolio(a).spendableBalance >= fee)
              .getOrElse(feeAccount)

            // Effective fee sponsor. None if the fee is paid by the sender.
            val feeSponsor = if (feePayer == tx.sender.toAddress) None else Some(feePayer)

            Monoid.combine(d, Diff.fee(tx, feeSponsor, Map(feePayer -> Portfolio(-fee))))
          }
      }
      positiveDiff <- BalanceDiffValidation(blockchain, currentBlockHeight, settings)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
