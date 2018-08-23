package one.legalthings.transaction.smart

import cats.syntax.all._
import one.legalthings.crypto
import one.legalthings.state._
import one.legalthings.transaction.ValidationError.{GenericError, ScriptExecutionError, TransactionNotAllowedByScript}
import one.legalthings.transaction._
import one.legalthings.transaction.assets._
import one.legalthings.transaction.smart.script.{Script, ScriptRunner}
import one.legalthings.transaction.transfer._

object Verifier {

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] =
    tx match {
      case _: GenesisTransaction => Right(tx)
      case pt: ProvenTransaction =>
        (pt, blockchain.accountScript(pt.sender)) match {
          case (_, Some(script))              => verify(blockchain, script, currentBlockHeight, pt, false)
          case (stx: SignedTransaction, None) => stx.signaturesValid()
          case _                              => verifyAsEllipticCurveSignature(pt)
        }
    }

  def verify[T <: Transaction](blockchain: Blockchain,
                               script: Script,
                               height: Int,
                               transaction: T,
                               isTokenScript: Boolean): Either[ValidationError, T] = {
    ScriptRunner[Boolean, T](height, transaction, blockchain, script) match {
      case (ctx, Left(execError)) => Left(ScriptExecutionError(script.text, execError, ctx.letDefs, isTokenScript))
      case (ctx, Right(false)) =>
        Left(TransactionNotAllowedByScript(ctx.letDefs, script.text, isTokenScript))
      case (_, Right(true)) => Right(transaction)
    }
  }

  def verifyAsEllipticCurveSignature[T <: ProvenTransaction](pt: T): Either[ValidationError, T] =
    pt.proofs.proofs match {
      case p :: Nil =>
        Either.cond(crypto.verify(p.arr, pt.bodyBytes(), pt.sender.publicKey),
                    pt,
                    GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }

}
