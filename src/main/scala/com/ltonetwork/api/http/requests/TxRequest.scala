package com.ltonetwork.api.http.requests

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.{Transaction, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet

trait TxRequest[TransactionT <: Transaction] {
  val sender: Option[String]
  val senderPublicKey: Option[String]

  def toTxFrom(sender: PublicKeyAccount): Either[ValidationError, TransactionT]

  def toTx: Either[ValidationError, TransactionT] =
    for {
      sender <- senderPublicKey match {
        case Some(key) => PublicKeyAccount.fromBase58String(key)
        case None      => Left(ValidationError.InvalidPublicKey("invalid.senderPublicKey"))
      }
      tx <- toTxFrom(sender)
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransactionT]

  def signTx(wallet: Wallet, time: Time): Either[ValidationError, TransactionT] = for {
    signerAddress <- sender.toRight(GenericError("invalid.sender"))
    tx <- signTx(wallet, signerAddress, time)
  } yield tx
}
