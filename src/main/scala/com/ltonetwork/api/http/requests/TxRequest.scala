package com.ltonetwork.api.http.requests

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.{Transaction, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet

trait TxRequest {
  def sender: Option[String]
  def senderPublicKey: Option[String]

  def toTx(sender: PublicKeyAccount): Either[ValidationError, Transaction]

  def toTx: Either[ValidationError, Transaction] =
    for {
      sender <- senderPublicKey match {
        case Some(key) => PublicKeyAccount.fromBase58String(key)
        case None      => Left(ValidationError.InvalidPublicKey("invalid.senderPublicKey"))
      }
      tx <- toTx(sender)
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, Transaction]

  def signTx(wallet: Wallet, time: Time): Either[ValidationError, Transaction] = for {
    signerAddress <- sender.toRight(GenericError("invalid.sender"))
    tx <- signTx(wallet, signerAddress, time)
  } yield tx
}
