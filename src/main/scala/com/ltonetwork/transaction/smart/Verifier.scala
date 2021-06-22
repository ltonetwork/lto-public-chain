package com.ltonetwork.transaction.smart

import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.ValidationError.{GenericError, ScriptExecutionError, TransactionNotAllowedByScript}
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.Transaction.SigProofsSwitch
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.smart.script.{Script, ScriptRunner}

object Verifier {

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] =
    tx match {
      case _: GenesisTransaction => Right(tx)
      case _ =>
        (tx, blockchain.accountScript(tx.sender)) match {
          case (stx: SigProofsSwitch, Some(_)) if stx.usesLegacySignature => Left(GenericError("Can't process transaction with signature from scripted account"))
          case (_, Some(script)) => verify(blockchain, script, currentBlockHeight, tx, isTokenScript = false)
          case (_, None)         => verifyAsEllipticCurveSignature(tx)
        }
    }

  def verify[T <: Transaction](blockchain: Blockchain,
                               script: Script,
                               height: Int,
                               transaction: T,
                               isTokenScript: Boolean): Either[ValidationError, T] = {
    ScriptRunner[Boolean, T](height, transaction, blockchain, script) match {
      case (log, Left(execError)) => Left(ScriptExecutionError(execError, script.text, log, isTokenScript))
      case (log, Right(false))    => Left(TransactionNotAllowedByScript(log, script.text, isTokenScript))
      case (_, Right(true))       => Right(transaction)
    }
  }

  def verifyAsEllipticCurveSignature[T <: Transaction](pt: T): Either[ValidationError, T] =
    pt.proofs.proofs match {
      case p :: Nil =>
        Either.cond(crypto.verify(p.arr, pt.bodyBytes(), pt.sender.publicKey),
                    pt,
                    GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }

}
