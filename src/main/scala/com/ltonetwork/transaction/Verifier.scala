package com.ltonetwork.transaction

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction.Transaction.SigProofsSwitch
import com.ltonetwork.transaction.ValidationError.{GenericError, ScriptExecutionError, TransactionNotAllowedByScript, valid}
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.smart.script.{Script, ScriptRunner}

object Verifier {

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] = {
    tx match {
      case _: GenesisTransaction => Right(tx)
      case _ =>
        (tx, blockchain.accountScript(tx.sender), tx.sponsor.flatMap(blockchain.accountScript(_))) match {
          case (stx: SigProofsSwitch, Some(_), _) if stx.usesLegacySignature =>
            Left(GenericError("Can't process transaction with signature from scripted account"))
          case (_, _, Some(_)) =>
            Left(GenericError(s"Transaction can't be sponsored by a scripted account"))
          case (_, Some(script), None) => verifySmartAccount(blockchain, script, currentBlockHeight, tx)
          case (_, None, None)         => verifyBasicAccount(tx)
        }
    }
  }

  def verifySmartAccount[T <: Transaction](blockchain: Blockchain,
                                           script: Script,
                                           height: Int,
                                           tx: T): Either[ValidationError, T] = for {
    _ <- verifyScript(blockchain, script, height, tx)
    _ <- tx.sponsor.fold(valid(tx))(verifySignature(tx, _))
  } yield tx

  private def verifyScript[T <: Transaction](blockchain: Blockchain,
                                             script: Script,
                                             height: Int,
                                             tx: T): Either[ValidationError, T] = {
    ScriptRunner[Boolean, T](height, tx, blockchain, script) match {
      case (log, Left(execError)) => Left(ScriptExecutionError(execError, script.text, log, isTokenScript = false))
      case (log, Right(false))    => Left(TransactionNotAllowedByScript(log, script.text, isTokenScript = false))
      case (_, Right(true))       => Right(tx)
    }
  }

  def verifyBasicAccount[T <: Transaction](tx: T): Either[ValidationError, T] =
    (tx.sponsor, tx.proofs.length) match {
      case (None, 1) => verifySignature(tx, tx.sender)
      case (None, _) => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
      case (Some(sponsor), 2) => for {
        _ <- verifySignature(tx, tx.sender)
        _ <- verifySignature(tx, sponsor)
      } yield tx
      case (Some(_), _) => Left(GenericError("Sponsored transactions from non-scripted accounts must have exactly 2 proofs"))
    }

  private def verifySignature[T <: Transaction](tx: T, account: PublicKeyAccount): Either[ValidationError, T] =
    Either.cond(tx.proofs.exists((proof: ByteStr) => crypto.verify(proof.arr, tx.bodyBytes(), account)),
      tx,
      GenericError(s"Proof doesn't validate as signature of $account for $tx"))
}
