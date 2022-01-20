package com.ltonetwork.transaction

import com.ltonetwork.settings.{Constants, FeesSettings, FunctionalitySettings}
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.transaction.transfer.MassTransferTransaction

class OldFeeCalculator(settings: FeesSettings, blockchain: Blockchain) {

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
          // variable fee is calculated per 256KB
          val dataSize =
            if (tx.data.nonEmpty) (tx.data.map(_.toBytes.length).sum / (1024*256)) + 1
            else 0
          mapVar
            .get(DataTransaction.typeId)
            .toRight(GenericError("Variable fee is not defined for DataTransaction"))
            .map(varFee => txMinBaseFee + varFee * dataSize)
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
}
