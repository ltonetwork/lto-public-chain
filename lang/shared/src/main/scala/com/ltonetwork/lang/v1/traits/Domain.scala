package com.ltonetwork.lang.v1.traits

import scodec.bits.ByteVector

case class Header(id: ByteVector, fee: Long, timestamp: Long, version: Long)

case class Proven(h: Header, sender: LangAddress, bodyBytes: ByteVector, senderPk: ByteVector, proofs: IndexedSeq[ByteVector])

case class LangAddress(bytes: ByteVector)

case class TransferItem(recipient: LangAddress, amount: Long)

trait Tx
object Tx {
  case class Genesis(header: Header, amount: Long, recipient: LangAddress)                     extends Tx
  case class Transfer(p: Proven, amount: Long, recipient: LangAddress, attachment: ByteVector) extends Tx
  case class Lease(p: Proven, amount: Long, recipient: LangAddress)                                                                       extends Tx
  case class LeaseCancel(p: Proven, leaseId: ByteVector)                                                                                  extends Tx
  case class SetScript(p: Proven, script: Option[ByteVector])                                                                             extends Tx
  case class MassTransfer(p: Proven, transferCount: Long, totalAmount: Long, transfers: IndexedSeq[TransferItem], attachment: ByteVector) extends Tx
  case class Anchor(p: Proven)                                                                                                            extends Tx
}
