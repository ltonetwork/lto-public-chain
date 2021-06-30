package com.ltonetwork.state.diffs

import cats.kernel.Monoid
import com.ltonetwork.account.Address
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.UnsupportedTransactionType
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.smart.{SetScriptTransaction, Verifier}
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer._

object TransactionDiffer {

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError

  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: Transaction): Either[ValidationError, Diff] = {
    for {
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
            case ltx: CancelLeaseTransaction  =>
              LeaseTransactionsDiff.leaseCancel(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
            case dtx: DataTransaction               => DataTransactionDiff(blockchain, currentBlockHeight)(dtx)
            case sstx: SetScriptTransaction         => SetScriptTransactionDiff(currentBlockHeight)(sstx)
            case at: AnchorTransaction              => AnchorTransactionDiff(blockchain, currentBlockHeight)(at)
            case as: AssociationTransaction         => AssociationTransactionDiff(currentBlockHeight)(as)
            case stx: SponsorshipTransaction        => SponsorshipTransactionDiff.sponsor(blockchain, currentBlockHeight)(stx)
            case sctx: CancelSponsorshipTransaction => SponsorshipTransactionDiff.cancel(blockchain, currentBlockHeight)(sctx)
            case _                                  => Left(UnsupportedTransactionType)
          }).map { d: Diff =>
            val feeAccount: Address = tx.sponsor.getOrElse(tx.sender)
            val feePayer: Address = blockchain
              .sponsorOf(feeAccount)
              .find(a => blockchain.portfolio(a).spendableBalance >= tx.fee)
              .getOrElse(tx.sender.toAddress)
            Monoid.combine(d, Diff.empty.copy(portfolios = Map(feePayer -> Portfolio(-tx.fee))))

            // TODO; if (feePayer != tx.sender) index fee payer of tx
          }
      }
      positiveDiff <- BalanceDiffValidation(blockchain, currentBlockHeight, settings)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
