package com.ltonetwork

import com.ltonetwork.api.http.requests._
import com.ltonetwork.state._
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.Base58
import org.scalacheck.Gen.{alphaNumChar, choose, listOfN, oneOf}
import org.scalacheck.{Arbitrary, Gen => G}
import org.scalatest.Suite
import scorex.crypto.signatures.Curve25519._

trait RequestGen extends TransactionGen { _: Suite =>
  val nonPositiveLong: G[Long] = choose(Long.MinValue, 0).label("non-positive value")
  val longAttachment: G[String] =
    genBoundedBytes(TransferTransaction.MaxAttachmentSize + 10, TransferTransaction.MaxAttachmentSize + 50)
      .map(Base58.encode)
  val invalidBase58: G[String] = listOfN(50, oneOf(alphaNumChar, oneOf('O', '0', 'l')))
    .map(_.mkString)
    .label("invalid base58")
  val invalidName: G[String] = oneOf(
    genBoundedString(0, 4 - 1),
    genBoundedString(140 + 1, 1000 + 50)
  ).map(new String(_))

  val addressValGen: G[String] = listOfN(32, Arbitrary.arbByte.arbitrary).map(b => Base58.encode(b.toArray))
  val signatureGen: G[ByteStr] = listOfN(SignatureLength, Arbitrary.arbByte.arbitrary)
    .map(b => ByteStr(b.toArray))

  private val commonFields = for {
    _account <- addressValGen
    _fee     <- smallFeeGen
  } yield (_account, _fee)

  val transferReq: G[TransferRequest] = for {
    (account, fee) <- commonFields
    recipient      <- addressValGen
    amount         <- positiveLongGen
    attachment     <- genBoundedString(1, 20).map(b => Some(ByteStr(b)))
  } yield TransferRequest(Some(1), None, Some(account), None, fee, recipient, amount, attachment)

  val broadcastTransferReq: G[TransferRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _tr        <- transferReq
  } yield TransferRequest(Some(1), Some(_timestamp), _tr.sender, None, _tr.fee, _tr.recipient, _tr.amount, _tr.attachment, Some(_signature))

  val leaseReq: G[LeaseRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _lease     <- leaseGen
  } yield LeaseRequest(Some(1), Some(_timestamp), Some(_lease.sender.toString), None, _lease.fee, _lease.recipient.toString, _lease.amount, Some(_signature))

  val leaseCancelReq: G[CancelLeaseRequest] = for {
    _signature <- signatureGen
    _cancel    <- leaseCancelGen
  } yield CancelLeaseRequest(Some(1), Some(_cancel.timestamp), Some(_cancel.sender.toString), None, _cancel.fee, _cancel.leaseId, Some(_signature))
}
