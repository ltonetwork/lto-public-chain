package com.ltonetwork

import cats.data.ValidatedNel
import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.utils.base58Length
import com.ltonetwork.block.{Block, MicroBlock}

package object transaction {
  type AssetId = com.ltonetwork.state.ByteStr

  val AssetIdLength: Int       = com.ltonetwork.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)

  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]

  implicit class TransactionValidationOps[T <: Transaction](val tx: T) extends AnyVal {
    def validatedNel(implicit validator: TxValidator[T]): ValidatedNel[ValidationError, T] = validator.validate(tx)
    def validatedEither(implicit validator: TxValidator[T]): Either[ValidationError, T]    = this.validatedNel.toEither.left.map(_.head)
  }

  implicit class TransactionSignOps[T](val tx: T) extends AnyVal {
    def signWith(privateKey: PrivateKeyAccount)(implicit sign: (T, PrivateKeyAccount) => T): T = sign(tx, privateKey)
  }
}
