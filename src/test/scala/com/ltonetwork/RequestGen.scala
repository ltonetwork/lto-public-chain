package com.ltonetwork

import com.ltonetwork.account.Alias
import com.ltonetwork.api.http.requests.lease.{SignedCancelLeaseV1Request, SignedLeaseV1Request}
import com.ltonetwork.api.http.requests.transfer.{SignedTransferV1Request, TransferV1Request}
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

  val invalidAliasStringByLength: G[String] = oneOf(
    G.choose(0, Alias.MinLength - 1) flatMap { sz =>
      G.listOfN(sz, G.alphaNumChar)
    },
    G.choose(Alias.MaxLength + 1, Alias.MaxLength + 50) flatMap { sz =>
      G.listOfN(sz, G.alphaNumChar)
    }
  ).map(_.mkString)

  val addressGen: G[String] = listOfN(32, Arbitrary.arbByte.arbitrary).map(b => Base58.encode(b.toArray))
  val signatureGen: G[String] = listOfN(SignatureLength, Arbitrary.arbByte.arbitrary)
    .map(b => Base58.encode(b.toArray))

  private val commonFields = for {
    _account <- addressGen
    _fee     <- smallFeeGen
  } yield (_account, _fee)

  val transferReq: G[TransferV1Request] = for {
    (account, fee) <- commonFields
    recipient      <- accountOrAliasGen.map(_.stringRepr)
    amount         <- positiveLongGen
    assetId    = None
    feeAssetId = None
    attachment <- genBoundedString(1, 20).map(b => Some(Base58.encode(b)))
  } yield TransferV1Request(assetId, feeAssetId, amount, fee, account, attachment, recipient)

  val broadcastTransferReq: G[SignedTransferV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _tr        <- transferReq
  } yield SignedTransferV1Request(_tr.sender, _tr.recipient, _tr.amount, _tr.fee, _timestamp, _tr.attachment, _signature)

  val leaseReq: G[SignedLeaseV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _alias     <- leaseGen
  } yield SignedLeaseV1Request(_alias.sender.toString, _alias.amount, _alias.fee, _alias.recipient.toString, _timestamp, _signature)

  val leaseCancelReq: G[SignedCancelLeaseV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _cancel    <- leaseCancelGen
  } yield SignedCancelLeaseV1Request(_cancel.sender.toString, _cancel.leaseId.base58, _cancel.timestamp, _signature, _cancel.fee)
}
