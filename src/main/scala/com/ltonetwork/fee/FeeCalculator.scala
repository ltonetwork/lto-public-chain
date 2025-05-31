package com.ltonetwork.fee

import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.features.FeatureProvider._
import com.ltonetwork.settings.Constants
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.{InsufficientFee, UnsupportedTransactionType}
import com.ltonetwork.transaction.anchor.{AnchorTransaction, MappedAnchorTransaction}
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.burn.BurnTransaction
import com.ltonetwork.transaction.certificate.CertificateTransaction
import com.ltonetwork.transaction.statement.StatementTransaction
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction, SponsorshipTransactionBase}
import com.ltonetwork.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.ltonetwork.transaction.{Transaction, ValidationError}
import com.ltonetwork.utils._

import scala.language.postfixOps
import scala.util.{Left, Right}

class FeeCalculator(blockchain: Blockchain) {
  private def dataBytes(data: List[DataEntry[_]], unitSize: Int): Integer =
    if (data.nonEmpty) ((data.map(_.toBytes.length).sum - 1) / unitSize) + 1
    else 0

  private def feesV5(height: Int, tx: Transaction): Either[ValidationError, Long] = (tx match {
    case _: GenesisTransaction            => Right(0)
    case _: TransferTransaction           => Right(1000)
    case _: LeaseTransaction              => Right(1000)
    case _: SetScriptTransaction          => Right(5000)
    case _: CancelLeaseTransaction        => Right(1000)
    case tx: MassTransferTransaction      => Right(1000 + tx.transfers.size * 100)
    case tx: AnchorTransaction            => Right(250 + tx.anchors.size * 100)
    case tx: IssueAssociationTransaction  => Right(500 + dataBytes(tx.data, 256) * 100)
    case _: RevokeAssociationTransaction  => Right(500)
    case _: SponsorshipTransaction        => Right(5000)
    case _: CancelSponsorshipTransaction  => Right(1000)
    case tx: DataTransaction              => Right(500 + dataBytes(tx.data, 256) * 100)
    case tx: RegisterTransaction          => Right(250 + tx.accounts.size * 100)
    case _: BurnTransaction               => Right(1000)
    case tx: MappedAnchorTransaction      => Right(250 + tx.anchors.size * 100)
    case tx: StatementTransaction         => Right(500 + dataBytes(tx.data, 256) * 100)
    case _: CertificateTransaction        => Right(5000)
    case _                                => Left(UnsupportedTransactionType)
  }).map(_ * blockchain.feePrice(height))

  private def feesV4(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction           => Right(0)
    case _: TransferTransaction          => Right(0.01 lto)
    case _: LeaseTransaction             => Right(0.01 lto)
    case _: SetScriptTransaction         => Right(0.1 lto)
    case _: CancelLeaseTransaction       => Right(0.01 lto)
    case tx: MassTransferTransaction     => Right((0.01 lto) + tx.transfers.size * (0.001 lto))
    case tx: AnchorTransaction           => Right((0.01 lto) + tx.anchors.size * (0.001 lto))
    case _: AssociationTransaction       => Right(0.01 lto)
    case _: SponsorshipTransaction       => Right(0.1 lto)
    case _: CancelSponsorshipTransaction => Right(0.1 lto)
    case tx: DataTransaction             => Right((0.01 lto) + dataBytes(tx.data, 1024*256) * (0.001 lto))
    case tx: RegisterTransaction         => Right((0.01 lto) + tx.accounts.size * (0.001 lto))
    case _: BurnTransaction              => Right(0.01 lto)
    case tx: MappedAnchorTransaction     => Right((0.01 lto) + tx.anchors.size * (0.001 lto))
    case tx: StatementTransaction        => Right((0.01 lto) + dataBytes(tx.data, 1024*256) * (0.001 lto))
    case _: CertificateTransaction       => Right(0.1 lto)
    case _                               => Left(UnsupportedTransactionType)
  }

  private def feesV3(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction         => Right(0)
    case _: TransferTransaction        => Right(1 lto)
    case _: LeaseTransaction           => Right(1 lto)
    case _: SetScriptTransaction       => Right(1 lto)
    case _: CancelLeaseTransaction     => Right(1 lto)
    case tx: MassTransferTransaction   => Right((1 lto) + tx.transfers.size * (0.1 lto))
    case _: AnchorTransaction          => Right(0.35 lto)
    case tx: DataTransaction           => Right((1 lto) + dataBytes(tx.data, 1024*256) * (0.1 lto))
    case _: AssociationTransaction     => Right(1 lto)
    case _: SponsorshipTransactionBase => Right(5 lto)
    case _                             => Left(UnsupportedTransactionType)
  }

  private def feesV2(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: AnchorTransaction          => Right(0.1 lto)
    case tx: DataTransaction           => Right((0.1 lto) + dataBytes(tx.data, 1024*256) * (0.01 lto))
    case _                             => feesV3(tx)
  }

  private def feesV1(tx: Transaction): Either[ValidationError, Long] = tx match {
    case _: GenesisTransaction       => Right(0)
    case _: TransferTransaction      => Right(0.001 lto)
    case tx: MassTransferTransaction => Right((0.001 lto) + tx.transfers.size * (0.0005 lto))
    case _: LeaseTransaction         => Right(0.001 lto)
    case _: CancelLeaseTransaction   => Right(0.001 lto)
    case tx: DataTransaction         => Right((0.001 lto) + ((tx.bodyBytes().length - 1) / 1024) * (0.001 lto))
    case tx: AnchorTransaction       => Right((0.001 lto) + ((tx.bodyBytes().length - 1) / 1024) * (0.001 lto))
    case _: SetScriptTransaction     => Right(0.001 lto)
    case _                           => Left(UnsupportedTransactionType)
  }

  def fee(tx: Transaction): Long = fee(blockchain.height, tx)
  def minFee(tx: Transaction): Either[ValidationError, Long] = minFee(blockchain.height, tx)
  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = enoughFee(blockchain.height, tx)

  def fee(height: Int, tx: Transaction): Long =
    if (blockchain.isFeatureActivated(BlockchainFeatures.Juicy, height))
      feesV5(height, tx).getOrElse(tx.fee)
    else
      tx.fee

  def minFee(height: Int, tx: Transaction): Either[ValidationError, Long] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.Juicy, height))
      feesV5(height, tx)
    else if (blockchain.isFeatureActivated(BlockchainFeatures.Cobalt, height))
      feesV4(tx)
    else if (blockchain.isFeatureActivated(BlockchainFeatures.BurnFeeture, height))
      feesV3(tx)
    else if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts, height))
      feesV2(tx)
    else
      feesV1(tx)

  // There's no longer a difference between consensus min fee and node min fee
  def consensusMinFee(height: Int, tx: Transaction): Either[ValidationError, Long] = minFee(height, tx)

  def enoughFee[T <: Transaction](height: Int, tx: T): Either[ValidationError, T] = {
    for {
      minTxFee <- minFee(height, tx)
      _ <- Either.cond(tx.fee >= minTxFee, (), insufficientFee(tx, minTxFee))
    } yield tx
  }

  private def insufficientFee[T <: Transaction](tx: T, minTxFee: Long): InsufficientFee = {
    val txName     = Constants.TransactionNames.getOrElse(tx.typeId, "unknown")
    val txFeeValue = tx.fee

    InsufficientFee(s"Fee for $txName transaction ($txFeeValue) does not exceed minimal value of $minTxFee")
  }
}

object FeeCalculator {
  def apply(blockchain: Blockchain): FeeCalculator = new FeeCalculator(blockchain)
}
