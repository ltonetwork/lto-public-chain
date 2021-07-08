package com.ltonetwork.transaction

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.Transaction.SigProofsSwitch
import com.ltonetwork.transaction.ValidationError.{GenericError, ScriptExecutionError, TransactionNotAllowedByScript}
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.smart.script.{Script, ScriptRunner}

object Verifier {

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] = {
    tx match {
      case _: GenesisTransaction => Right(tx)
      case _ =>
        (tx, blockchain.accountScript(tx.sender)) match {
          case (stx: SigProofsSwitch, Some(_)) if stx.usesLegacySignature =>
            Left(GenericError("Can't process transaction with signature from scripted account"))
          case (_, Some(script)) => verifySmartAccount(blockchain, script, currentBlockHeight, tx)
          case (_, None)         => verifyBasicAccount(tx)
        }
    }
  }

  def verifySmartAccount[T <: Transaction](blockchain: Blockchain,
                                           script: Script,
                                           height: Int,
                                           transaction: T): Either[ValidationError, T] =
    verifyScript(blockchain, script, height, transaction)


  private def verifyScript[T <: Transaction](blockchain: Blockchain,
                               script: Script,
                               height: Int,
                               transaction: T): Either[ValidationError, T] = {
    ScriptRunner[Boolean, T](height, transaction, blockchain, script) match {
      case (log, Left(execError)) => Left(ScriptExecutionError(execError, script.text, log, isTokenScript = false))
      case (log, Right(false))    => Left(TransactionNotAllowedByScript(log, script.text, isTokenScript = false))
      case (_, Right(true))       => Right(transaction)
    }
  }

  def verifyBasicAccount[T <: Transaction](tx: T): Either[ValidationError, T] = {
    (tx.sponsor, tx.proofs.length) match {
      case (None, 1) => verifySignature(tx, tx.sender)
      case (None, _) => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
      case (Some(sponsor), 2) => for {
        _ <- verifySignature(tx, tx.sender)
        vtx <- verifySignature(tx, sponsor)
      } yield vtx
      case (Some(_), _) => Left(GenericError("Sponsored transactions from non-scripted accounts must have exactly 2 proofs"))
    }
  }

  private def verifySignature[T <: Transaction](tx: T, account: PublicKeyAccount): Either[ValidationError, T] =
    Either.cond(tx.proofs.exists((proof: ByteStr) => crypto.verify(proof.arr, tx.bodyBytes(), account.publicKey)),
      tx,
      GenericError(s"Proof doesn't validate as signature of $account for $tx"))
}
