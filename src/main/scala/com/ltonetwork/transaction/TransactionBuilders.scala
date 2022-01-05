package com.ltonetwork.transaction

import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.association.{IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.base58Length
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Success, Try}

object TransactionBuilders {

  val SignatureStringLength: Int = base58Length(Curve25519.SignatureLength)

  val all: Map[Byte, TransactionBuilder] = Seq[TransactionBuilder](
    GenesisTransaction,
    TransferTransaction,
    MassTransferTransaction,
    SetScriptTransaction,
    LeaseTransaction,
    CancelLeaseTransaction,
    DataTransaction,
    AnchorTransaction,
    IssueAssociationTransaction,
    RevokeAssociationTransaction,
    SponsorshipTransaction,
    CancelSponsorshipTransaction,
    RegisterTransaction
  ).map { x =>
    x.typeId -> x
  }(collection.breakOut)

  def by(typeId: Byte): Option[TransactionBuilder] = all.get(typeId)

  def parseBytes(data: Array[Byte]): Try[Transaction] =
    data.headOption
      .fold[Try[Byte]](Failure(new IllegalArgumentException("Can't find the significant byte: the buffer is empty")))(Success(_))
      .flatMap { headByte =>
        if (headByte == 0) modernParseBytes(data)
        else oldParseBytes(headByte, data)
      }

  private def oldParseBytes(typeId: Byte, data: Array[Byte]): Try[Transaction] =
    all
      .get(typeId)
      .fold[Try[TransactionBuilder]](Failure(new IllegalArgumentException(s"Unknown transaction type (old encoding): $typeId")))(Success(_))
      .flatMap(_.parseBytes(data))

  private def modernParseBytes(data: Array[Byte]): Try[Transaction] = {
    if (data.length < 2)
      Failure(new IllegalArgumentException(s"Can't determine the type and the version of transaction: the buffer has ${data.length} bytes"))
    else {
      val Array(_, typeId) = data.take(2)
      all
        .get(typeId)
        .fold[Try[TransactionBuilder]](Failure(new IllegalArgumentException(s"Unknown transaction type: $typeId")))(Success(_))
        .flatMap(_.parseBytes(data))
    }
  }
}
