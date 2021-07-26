package com.ltonetwork.transaction

import com.ltonetwork.settings.{Constants, FeesSettings, FunctionalitySettings}
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.transfer.MassTransferTransaction

class FeeCalculator(settings: FeesSettings, blockchain: Blockchain) {

  private val Kb = 1024

  val map: Map[String, Long] = {
    settings.fees.flatMap { fs =>
      val transactionType = fs._1
      fs._2.filter(v => v.asset.toUpperCase() == "BASE").map { v =>
        transactionType.toString -> v.fee
      }
    }
  }

  val mapVar: Map[String, Long] = {
    settings.fees.flatMap { fs =>
      val transactionType = fs._1
      fs._2.filter(v => v.asset.toUpperCase() == "VAR").map { v =>
        transactionType.toString -> v.fee
      }
    }
  }

  def enoughFee[T <: Transaction](tx: T, blockchain: Blockchain, fs: FunctionalitySettings): Either[ValidationError, T] = enoughFee(tx)

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = {
    val txName        = Constants.TransactionNames(tx.typeId)
    val txFeeValue    = tx.fee
    val txAssetFeeKey = tx.builder.typeId.toString
    for {
      txMinBaseFee <- Either.cond(map.contains(txAssetFeeKey), map(txAssetFeeKey), GenericError(s"Minimum fee is not defined for $txAssetFeeKey"))
      minTxFee = minFeeFor(tx, txMinBaseFee)
      _ <- Either.cond(
        txFeeValue >= minTxFee,
        (),
        GenericError {
          s"Fee for ${txName} transaction ($txFeeValue LTO) does not exceed minimal value of $minTxFee LTO"
        }
      )
    } yield tx
  }

  private def minFeeFor(tx: Transaction, txMinBaseFee: Long): Long = tx match {
    case tx: DataTransaction =>
      val sizeInKb = 1 + (tx.bytes().length - 1) / Kb
      txMinBaseFee * sizeInKb
    case tx: AnchorTransaction =>
      val varFee = mapVar.getOrElse(AnchorTransaction.typeId.toString, throw new IllegalStateException("Can't find variable fee for AnchorTransaction"))
      txMinBaseFee + varFee * tx.anchors.size
    case tx: MassTransferTransaction =>
      val varFee = mapVar.getOrElse(MassTransferTransaction.typeId.toString, throw new IllegalStateException("Can't find variable fee for MassTransferTransaction"))
      txMinBaseFee + varFee * tx.transfers.size
    case _ => txMinBaseFee
  }
}
