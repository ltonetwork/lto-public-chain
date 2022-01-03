package com.ltonetwork.transaction

import com.ltonetwork.settings.{Constants, FeesSettings, FunctionalitySettings}
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.{GenericError, InsufficientFee}
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.transaction.transfer.MassTransferTransaction

class FeeCalculator(settings: FeesSettings, blockchain: Blockchain) {

  private val Kb = 1024

  private val map: Map[Byte, Long] = {
    settings.fees.flatMap { fs =>
      val transactionType = fs._1
      fs._2.filter(v => v.asset.toUpperCase() == "BASE").map { v =>
        transactionType -> v.fee
      }
    }
  }

  private val mapVar: Map[Byte, Long] = {
    settings.fees.flatMap { fs =>
      val transactionType = fs._1
      fs._2.filter(v => v.asset.toUpperCase() == "VAR").map { v =>
        transactionType -> v.fee
      }
    }
  }

  def minFee(tx: Transaction, blockchain: Blockchain, fs: FunctionalitySettings): Either[ValidationError, Long] = minFee(tx)

  def minFee(tx: Transaction): Either[ValidationError, Long] = {
    val txName = Constants.TransactionNames(tx.typeId)
    for {
      txMinBaseFee <- Either.cond(map.contains(tx.typeId), map(tx.typeId), GenericError(s"Minimum fee is not defined for $txName"))
      minTxFee <- tx match {
        case tx: DataTransaction =>
          val sizeInKb = 1 + (tx.bytes().length - 1) / Kb
          Right(txMinBaseFee * sizeInKb)
        case tx: AnchorTransaction =>
          mapVar
            .get(AnchorTransaction.typeId)
            .toRight(GenericError("Variable fee is not defined for AnchorTransaction"))
            .map(varFee => txMinBaseFee + varFee * tx.anchors.size)
        case tx: MassTransferTransaction =>
          mapVar
            .get(MassTransferTransaction.typeId)
            .toRight(GenericError("Can't find variable fee for MassTransferTransaction"))
            .map(varFee => txMinBaseFee + varFee * tx.transfers.size)
        case tx: RegisterTransaction =>
          mapVar
            .get(RegisterTransaction.typeId)
            .toRight(GenericError("Variable fee is not defined for RegisterTransaction"))
            .map(varFee => txMinBaseFee + varFee * tx.accounts.size)
        case _ =>
          Right(txMinBaseFee)
      }
    } yield minTxFee
  }

  def enoughFee[T <: Transaction](tx: T, blockchain: Blockchain, fs: FunctionalitySettings): Either[ValidationError, T] = enoughFee(tx)

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = {
    val txName     = Constants.TransactionNames(tx.typeId)
    val txFeeValue = tx.fee

    for {
      minTxFee <- minFee(tx)
      _ <- Either.cond(
        txFeeValue >= minTxFee,
        (),
        InsufficientFee(s"Fee for ${txName} transaction ($txFeeValue) does not exceed minimal value of $minTxFee")
      )
    } yield tx
  }
}
