package com.ltonetwork.transaction

import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.association.{IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.base58Length
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Success, Try}

object TransactionBuilders {

  val TimestampLength            = 8
  val AmountLength               = 8
  val TypeLength                 = 1
  val SignatureStringLength: Int = base58Length(Curve25519.SignatureLength)

  private val old: Map[Byte, TransactionBuilder] = Seq[TransactionBuilder](
    GenesisTransaction,
    TransferTransaction,
    LeaseTransaction,
    CancelLeaseTransaction,
    MassTransferTransaction
  ).map { x =>
    x.typeId -> x
  }(collection.breakOut)

  private val modern: Map[(Byte, Byte), TransactionBuilder] = Seq[TransactionBuilder](
    AnchorTransaction,
    DataTransaction,
    TransferTransaction,
    SetScriptTransaction,
    LeaseTransaction,
    CancelLeaseTransaction,
    IssueAssociationTransaction,
    RevokeAssociationTransaction,
    SponsorshipTransaction,
    CancelSponsorshipTransaction
  ).flatMap { x =>
    x.supportedVersions.map { version =>
      ((x.typeId, version), x)
    }
  }(collection.breakOut)

  private val all: Map[(Byte, Byte), TransactionBuilder] = old.flatMap {
    case (typeId, builder) =>
      builder.supportedVersions.map { version =>
        ((typeId, version), builder)
      }
  } ++ modern

  def by(typeId: Byte, version: Byte): Option[TransactionBuilder] = all.get((typeId, version))

  def parseBytes(data: Array[Byte]): Try[Transaction] =
    data.headOption
      .fold[Try[Byte]](Failure(new IllegalArgumentException("Can't find the significant byte: the buffer is empty")))(Success(_))
      .flatMap { headByte =>
        if (headByte == 0) modernParseBytes(data)
        else oldParseBytes(headByte, data)
      }

  private def oldParseBytes(tpe: Byte, data: Array[Byte]): Try[Transaction] =
    old
      .get(tpe)
      .fold[Try[TransactionBuilder]](Failure(new IllegalArgumentException(s"Unknown transaction type (old encoding): '$tpe'")))(Success(_))
      .flatMap(_.parseBytes(data))

  private def modernParseBytes(data: Array[Byte]): Try[Transaction] = {
    if (data.length < 2)
      Failure(new IllegalArgumentException(s"Can't determine the type and the version of transaction: the buffer has ${data.length} bytes"))
    else {
      val Array(_, typeId, version) = data.take(3)
      modern
        .get((typeId, version))
        .fold[Try[TransactionBuilder]](
          Failure(new IllegalArgumentException(s"Unknown transaction type ($typeId) and version ($version) (modern encoding)")))(Success(_))
        .flatMap(_.parseBytes(data))
    }
  }
}
