package com.ltonetwork.lang.v1.traits

import scodec.bits.ByteVector

case class Header(id: ByteVector, fee: Long, timestamp: Long, version: Long)

case class Proven(h: Header, sender: LangAddress, bodyBytes: ByteVector, senderPk: ByteVector, proofs: IndexedSeq[ByteVector])

case class LangAddress(bytes: ByteVector)

case class TransferItem(recipient: LangAddress, amount: Long)

case class PublicKey(bytes: ByteVector)

case class KeyValuePair(key: ByteVector, value: ByteVector)

trait DataItem[T] {
  val key: String
  val value: T
}

object DataItem {
  case class Lng(k: String, v: Long)       extends DataItem[Long]       { val key = k; val value = v }
  case class Bool(k: String, v: Boolean)   extends DataItem[Boolean]    { val key = k; val value = v }
  case class Bin(k: String, v: ByteVector) extends DataItem[ByteVector] { val key = k; val value = v }
  case class Str(k: String, v: String)     extends DataItem[String]     { val key = k; val value = v }
}

trait Tx
object Tx {
  case class Genesis(header: Header, amount: Long, recipient: LangAddress)                                                                extends Tx
  case class Transfer(p: Proven, amount: Long, recipient: LangAddress, attachment: ByteVector)                                            extends Tx
  case class Lease(p: Proven, amount: Long, recipient: LangAddress)                                                                       extends Tx
  case class CancelLease(p: Proven, leaseId: ByteVector)                                                                                  extends Tx
  case class SetScript(p: Proven, script: Option[ByteVector])                                                                             extends Tx
  case class MassTransfer(p: Proven, transferCount: Long, totalAmount: Long, transfers: IndexedSeq[TransferItem], attachment: ByteVector) extends Tx
  case class Anchor(p: Proven, anchorCount: Long, anchors: IndexedSeq[ByteVector])                                                        extends Tx
  case class MappedAnchor(p: Proven, anchorCount: Long, anchors: IndexedSeq[KeyValuePair])                                                extends Tx
  case class Data(p: Proven, data: IndexedSeq[DataItem[_]])                                                                               extends Tx
  case class IssueAssociation(p: Proven, assocType: Long, recipient: LangAddress)                                                         extends Tx
  case class RevokeAssociation(p: Proven, assocType: Long, recipient: LangAddress)                                                        extends Tx
  case class Sponsorship(p: Proven, recipient: LangAddress)                                                                               extends Tx
  case class CancelSponsorship(p: Proven, recipient: LangAddress)                                                                         extends Tx
  case class Register(p: Proven, accounts: IndexedSeq[PublicKey])                                                                         extends Tx
}
