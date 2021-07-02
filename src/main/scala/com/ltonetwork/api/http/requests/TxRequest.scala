package com.ltonetwork.api.http.requests

import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.{Transaction, ValidationError}
import com.ltonetwork.utils.Time
import com.ltonetwork.wallet.Wallet

trait TxRequest[TransactionT <: Transaction] {
  val sender: Option[String]
  val senderPublicKey: Option[String]
  val sponsor: Option[String]
  val sponsorPublicKey: Option[String]

  def toTxFrom(sender: PublicKeyAccount, sponsor: Option[PublicKeyAccount]): Either[ValidationError, TransactionT]

  def toTxFrom(sender: PublicKeyAccount): Either[ValidationError, TransactionT] =
    toTxFrom(sender, None)

  def toTx: Either[ValidationError, TransactionT] =
    for {
      sender <- senderPublicKey match {
        case Some(key) => PublicKeyAccount.fromBase58String(key)
        case None      => Left(ValidationError.InvalidPublicKey("invalid.senderPublicKey"))
      }
      sponsor <- sponsorPublicKey.map(key => PublicKeyAccount.fromBase58String(key))
        .fold[Either[ValidationError, Option[PublicKeyAccount]]](Right(None))(_.map(k => Some(k)))
      tx <- toTxFrom(sender, sponsor)
    } yield tx

  def signTx(wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransactionT]

  def signTx(wallet: Wallet, time: Time): Either[ValidationError, TransactionT] = for {
    signerAddress <- sender.toRight(GenericError("invalid.sender"))
    tx <- signTx(wallet, signerAddress, time)
  } yield tx

  def sponsorTx(wallet: Wallet, time: Time): Either[ValidationError, TransactionT] = for {
    signerAddress <- sponsor.toRight(GenericError("invalid.sponsor"))
    tx <- signTx(wallet, signerAddress, time)
  } yield tx

  // TODO: Create sender or sponsor account from public key
  protected def resolveAccounts(wallet: Wallet, signerAddress: String): Either[ValidationError, (PublicKeyAccount, Option[PublicKeyAccount], PrivateKeyAccount)] = for {
    senderAddress  <- sender.toRight(GenericError("invalid.sender"))
    senderAccount  <- wallet.findPrivateKey(senderAddress)
    sponsorAccount <- if (sponsor.isEmpty) Right(None) else wallet.findPrivateKey(sponsor.get).map(a => Some(a))
    signerAccount  <-
      if (signerAddress == senderAddress) Right(senderAccount)
      else if (sponsor.isDefined && signerAddress == sponsor.get) Right(sponsorAccount.get)
      else wallet.findPrivateKey(signerAddress)
  } yield (senderAccount, sponsorAccount, signerAccount)

}
