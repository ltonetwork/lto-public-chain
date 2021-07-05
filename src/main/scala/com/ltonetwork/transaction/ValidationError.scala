package com.ltonetwork.transaction

import com.google.common.base.Throwables
import com.ltonetwork.account.Address
import com.ltonetwork.block.{Block, MicroBlock}
import com.ltonetwork.lang.ExprEvaluator.Log
import com.ltonetwork.state._

import scala.util.Either

trait ValidationError

object ValidationError {
  type Validation[T] = Either[ValidationError, T]

  case class InvalidAddress(reason: String)                    extends ValidationError
  case class NegativeAmount(amount: Long, of: String)          extends ValidationError
  case class NegativeMinFee(minFee: Long, of: String)          extends ValidationError
  case class InsufficientFee(msg: String = "insufficient fee") extends ValidationError
  case object TooBigArray                                      extends ValidationError
  case object InvalidName                                      extends ValidationError
  case object OverflowError                                    extends ValidationError
  case object ToSelf                                           extends ValidationError
  case object MissingSenderPrivateKey                          extends ValidationError
  case object AccountBasedWallet                               extends ValidationError
  case object UnsupportedTransactionType                       extends ValidationError
  case object InvalidRequestSignature                          extends ValidationError
  case class BlockFromFuture(ts: Long)                         extends ValidationError
  case class ScriptParseError(m: String)                       extends ValidationError
  case class AlreadyInTheState(txId: ByteStr, txHeight: Int)   extends ValidationError
  case class AccountBalanceError(errs: Map[Address, String])   extends ValidationError
  case class SenderIsBlacklisted(addr: String)                 extends ValidationError
  case class Mistiming(err: String)                            extends ValidationError
  case class BlockAppendError(err: String, b: Block)           extends ValidationError
  case class ActivationError(err: String)                      extends ValidationError
  case class UnsupportedVersion(version: Int)                  extends ValidationError
  case class GenericError(err: String)                         extends ValidationError
  case class InvalidPublicKey(err: String)                     extends ValidationError
  case class UnsupportedFeature(err: String)                   extends ValidationError
  case class WrongChainId(chainId: Byte)                       extends ValidationError

  object GenericError {
    def apply(ex: Throwable): GenericError = new GenericError(Throwables.getStackTraceAsString(ex))
  }

  case class InvalidSignature(s: Signed, details: Option[InvalidSignature] = None) extends ValidationError {
    override def toString: String = s"InvalidSignature(${s.toString + " reason: " + details})"
  }

  case class ScriptExecutionError(error: String, scriptSrc: String, log: Log, isTokenScript: Boolean) extends ValidationError

  case class TransactionNotAllowedByScript(log: Log, scriptSrc: String, isTokenScript: Boolean) extends ValidationError

  case class MicroBlockAppendError(err: String, microBlock: MicroBlock) extends ValidationError {
    override def toString: String = s"MicroBlockAppendError($err, ${microBlock.totalResBlockSig} ~> ${microBlock.prevResBlockSig.trim}])"
  }

}
