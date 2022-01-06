package com.ltonetwork.transaction.smart

import com.ltonetwork.lang.v1.traits.{Proven, _}
import com.ltonetwork.state._
import scodec.bits.ByteVector
import com.ltonetwork.account.Address
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.transaction.transfer._

object RealTransactionWrapper {

  private def header(tx: Transaction): Header = {
    Header(ByteVector(tx.id().arr), tx.fee, tx.timestamp, tx.version)
  }

  private def proven(tx: Transaction): Proven =
    Proven(
      header(tx),
      LangAddress(ByteVector(tx.sender.bytes.arr)),
      ByteVector(tx.bodyBytes()),
      ByteVector(tx.sender.publicKey),
      tx.proofs.proofs.map(_.arr).map(ByteVector(_)).toIndexedSeq
    )

  implicit def toByteVector(s: ByteStr): ByteVector = ByteVector(s.arr)

  implicit def aoaToRecipient(a: Address): LangAddress = LangAddress(ByteVector(a.bytes.arr))

  def apply(tx: Transaction): Tx = {
    tx match {
      case g: GenesisTransaction => Tx.Genesis(header(g), g.amount, g.recipient)
      case t: TransferTransaction =>
        Tx.Transfer(
          proven(t),
          amount = t.amount,
          recipient = t.recipient,
          attachment = ByteVector(t.attachment),
        )

      case b: LeaseTransaction       => Tx.Lease(proven(b), b.amount, b.recipient)
      case b: CancelLeaseTransaction => Tx.LeaseCancel(proven(b), b.leaseId)
      case ms: MassTransferTransaction =>
        Tx.MassTransfer(
          proven(ms),
          transferCount = ms.transfers.length,
          totalAmount = ms.transfers.map(_.amount).sum,
          transfers = ms.transfers.map(r => com.ltonetwork.lang.v1.traits.TransferItem(r.address, r.amount)).toIndexedSeq,
          attachment = ByteVector(ms.attachment),
        )
      case ss: SetScriptTransaction => Tx.SetScript(proven(ss), ss.script.map(_.bytes()).map(toByteVector))
      case a: AnchorTransaction     => Tx.Anchor(proven(a))
      case r: RegisterTransaction =>
        Tx.Register(proven(r), r.accounts.map(k => com.ltonetwork.lang.v1.traits.PublicKey(ByteVector(k.publicKey))).toIndexedSeq)
    }
  }
}
