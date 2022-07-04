package com.ltonetwork

import cats.data.ValidatedNel
import com.ltonetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.block.{Block, MicroBlock}
import com.ltonetwork.transaction.burn.BurnTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer.{MassTransferTransaction, TransferTransaction}

package object transaction {
  type AssetId = com.ltonetwork.state.ByteStr

  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]


  def portfolioTxTypes: Set[Byte] = Set(
    GenesisTransaction.typeId,
    TransferTransaction.typeId,
    MassTransferTransaction.typeId,
    BurnTransaction.typeId,
    LeaseTransaction.typeId,
    CancelLeaseTransaction.typeId,
    SponsorshipTransaction.typeId,
    CancelSponsorshipTransaction.typeId,
  )

  implicit class TransactionValidationOps[T <: Transaction](val tx: T) extends AnyVal {
    def validatedNel(implicit validator: TxValidator[T]): ValidatedNel[ValidationError, T] = validator.validate(tx)
    def validatedEither(implicit validator: TxValidator[T]): Either[ValidationError, T]    = this.validatedNel.toEither.left.map(_.head)
  }

  implicit class TransactionSignOps[T](val tx: T) extends AnyVal {
    def signWith(privateKey: PrivateKeyAccount)(implicit sign: (T, PrivateKeyAccount) => T): T = sign(tx, privateKey)

    def sponsorWith(privateKey: PrivateKeyAccount)(implicit sign: (T, PrivateKeyAccount, Option[PublicKeyAccount]) => T): T =
      sign(tx, privateKey, Some(privateKey))
    def sponsorWith(maybePk: Option[PrivateKeyAccount])(implicit sign: (T, PrivateKeyAccount, Option[PublicKeyAccount]) => T): T = maybePk match {
      case None             => tx // sponsorWith(None) does *not* clear an existing sponsor
      case Some(privateKey) => sign(tx, privateKey, Some(privateKey))
    }
  }
}
