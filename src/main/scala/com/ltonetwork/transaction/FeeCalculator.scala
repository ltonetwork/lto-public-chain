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
    for {
      txMinBaseFee <- Either.cond(map.contains(tx.typeId), map(tx.typeId), noMinFee(tx.typeId))
      minTxFee <- tx match {
        case tx: DataTransaction =>
          val sizeInKb = 1 + (tx.bytes().length - 1) / Kb
          Right(txMinBaseFee * sizeInKb)
        case tx: AnchorTransaction =>
          mapVar
            .get(AnchorTransaction.typeId)
            .toRight(GenericError("Variable fee is not defined for anchor transaction"))
            .map(varFee => txMinBaseFee + varFee * tx.anchors.size)
        case tx: MassTransferTransaction =>
          mapVar
            .get(MassTransferTransaction.typeId)
            .toRight(GenericError("Variable fee is not defined for mass transfer transaction"))
            .map(varFee => txMinBaseFee + varFee * tx.transfers.size)
        case tx: RegisterTransaction =>
          mapVar
            .get(RegisterTransaction.typeId)
            .toRight(GenericError("Variable fee is not defined for register transaction"))
            .map(varFee => txMinBaseFee + varFee * tx.accounts.size)
        case _ =>
          Right(txMinBaseFee)
      }
    } yield minTxFee
  }

  private def noMinFee(typeId: Byte): GenericError = {
    val txName = Constants.TransactionNames.getOrElse(typeId, "unknown")
    GenericError(s"Minimum fee is not defined for $txName transaction")
  }

  def enoughFee[T <: Transaction](tx: T, blockchain: Blockchain, fs: FunctionalitySettings): Either[ValidationError, T] = enoughFee(tx)

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = {
    for {
      minTxFee <- minFee(tx)
      _ <- Either.cond(tx.fee >= minTxFee, (), insufficientFee(tx, minTxFee))
    } yield tx
  }

  private def insufficientFee[T <: Transaction](tx: T, minTxFee: Long): InsufficientFee = {
    val txName     = Constants.TransactionNames.getOrElse(tx.typeId, "unknown")
    val txFeeValue = tx.fee

    InsufficientFee(s"Fee for ${txName} transaction ($txFeeValue) does not exceed minimal value of $minTxFee")
  }
}
