package com.ltonetwork.api.requests

import cats.data.Validated
import com.ltonetwork.account.KeyTypes.keyType
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.settings._
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.{GenericError, InvalidPublicKey}
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.association.{IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.burn.BurnTransaction
import com.ltonetwork.transaction.statement.StatementTransaction
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.ltonetwork.transaction.{Proofs, Transaction, TransactionBuilders, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet
import play.api.libs.json.JsObject

trait TxRequest {
  type TransactionT <: Transaction

  val timestamp: Option[Long]
  val sender: Option[String]
  val senderKeyType: Option[String]
  val senderPublicKey: Option[String]
  val sponsor: Option[String]
  val sponsorKeyType: Option[String]
  val sponsorPublicKey: Option[String]
  val signature: Option[ByteStr]
  val proofs: Option[Proofs]

  protected def timestamp(time: Option[Time]): Long = timestamp.getOrElse(time.fold(defaultTimestamp)(_.getTimestamp()))

  implicit protected def sign(tx: TransactionT, signer: PrivateKeyAccount): TransactionT

  private def publicKeyAccount(keyTypeOpt: Option[String], publicKey: String): Either[ValidationError, PublicKeyAccount] =
    for {
      kt  <- keyType(keyTypeOpt).fold(t => Left(InvalidPublicKey(t.getMessage)), Right(_))
      acc <- PublicKeyAccount.fromBase58String(kt, publicKey)
    } yield acc

  protected def resolveSender: Either[ValidationError, PublicKeyAccount] = {
    for {
      account <- senderPublicKey match {
        case Some(key) => publicKeyAccount(senderKeyType, key)
        case None => Left(InvalidPublicKey("missing senderPublicKey"))
      }
      _ <- Validated.cond(sender.forall(account.address == _), (), InvalidPublicKey("senderPublicKey doesn't match sender address")).toEither
    } yield account
  }

  protected def resolveSender(wallet: Wallet): Either[ValidationError, PublicKeyAccount] =
    sender.map(wallet.findPrivateKey).getOrElse(resolveSender)

  protected def resolveSponsor: Either[ValidationError, Option[PublicKeyAccount]] = {
    for {
      account <- sponsorPublicKey
        .map(publicKeyAccount(sponsorKeyType, _))
        .fold[Either[ValidationError, Option[PublicKeyAccount]]](Right(None))(_.map(k => Some(k)))
      _ <- Validated.cond(
          account.isEmpty || sponsor.isEmpty || account.get.address == sponsor.get,
          (),
          InvalidPublicKey("sponsorPublicKey doesn't match sponsor address")
        ).toEither
    } yield account
  }

  protected def resolveSponsor(wallet: Wallet): Either[ValidationError, Option[PublicKeyAccount]] =
    sponsor.map(address => wallet.findPrivateKey(address).map(Some(_))).getOrElse(resolveSponsor)

  protected def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount], proofs: Proofs, timestamp: Long): Either[ValidationError, TransactionT]

  def toTx: Either[ValidationError, TransactionT] =
    for {
      senderAcc   <- resolveSender
      sponsorAcc  <- resolveSponsor
      validProofs <- toProofs(signature, proofs)
      tx          <- toTxFrom(senderAcc, sponsorAcc, validProofs, timestamp.getOrElse(defaultTimestamp))
    } yield tx

  def signTx(wallet: Wallet, time: Time): Either[ValidationError, TransactionT] =

    for {
      senderAcc   <- resolveSender(wallet)
      sponsorAcc  <- resolveSponsor(wallet)
      validProofs <- toProofs(signature, proofs)
      tx          <- toTxFrom(senderAcc, sponsorAcc, validProofs, timestamp.getOrElse(time.getTimestamp()))
    } yield tx.signMaybe(senderAcc).signMaybe(sponsorAcc)
}

object TxRequest {
  abstract class For[T <: Transaction] extends TxRequest {
    override type TransactionT = T
  }

  def fromJson(jsv: JsObject): Either[ValidationError, TxRequest] =
    fromJson((jsv \ "type").as[Byte], jsv)

  def fromJson(typeName: String, jsv: JsObject): Either[ValidationError, TxRequest] = {
    for {
      typeId    <- transactionTypes.get(typeName).toRight(GenericError(s"Bad transaction type ($typeName)"))
      txRequest <- fromJson(typeId, jsv)
    } yield txRequest
  }

  def fromJson(typeId: Byte, jsv: JsObject): Either[ValidationError, TxRequest] = {
    TransactionBuilders.by(typeId) match {
      case None => Left(GenericError(s"Bad transaction type ($typeId)"))
      case Some(x) =>
        x match {
          case AnchorTransaction            => Right(jsv.as[AnchorRequest])
          case IssueAssociationTransaction  => Right(jsv.as[IssueAssociationRequest])
          case RevokeAssociationTransaction => Right(jsv.as[RevokeAssociationRequest])
          case SponsorshipTransaction       => Right(jsv.as[SponsorshipRequest])
          case CancelSponsorshipTransaction => Right(jsv.as[CancelSponsorshipRequest])
          case TransferTransaction          => Right(jsv.as[TransferRequest])
          case MassTransferTransaction      => Right(jsv.as[MassTransferRequest])
          case LeaseTransaction             => Right(jsv.as[LeaseRequest])
          case CancelLeaseTransaction       => Right(jsv.as[CancelLeaseRequest])
          case DataTransaction              => Right(jsv.as[DataRequest])
          case SetScriptTransaction         => Right(jsv.as[SetScriptRequest])
          case RegisterTransaction          => Right(jsv.as[RegisterRequest])
          case BurnTransaction              => Right(jsv.as[BurnRequest])
          case StatementTransaction             => Right(jsv.as[StatementRequest])
          case _                            => Left(GenericError(s"Unsupported transaction type ($typeId)"))
        }
    }
  }
}
