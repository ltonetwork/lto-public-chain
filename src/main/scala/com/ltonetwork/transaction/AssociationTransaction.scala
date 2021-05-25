package com.ltonetwork.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.{Address, AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.AssociationTransaction.{ActionType, Assoc}
import com.ltonetwork.transaction.ValidationError.GenericError
import monix.eval.Coeval
import play.api.libs.json._
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}
abstract class AssociationTransactionBase(val version: Byte,
                                          val chainId: Byte,
                                          val sender: PublicKeyAccount,
                                          val assoc: Assoc,
                                          val fee: Long,
                                          val timestamp: Long,
                                          val proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  def actionType: AssociationTransaction.ActionType
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(builder.typeId, version, chainId),
      sender.publicKey,
      assoc.party.bytes.arr,
      Ints.toByteArray(assoc.assocType),
      assoc.hash.map(a => (1: Byte) +: Deser.serializeArray(a.arr)).getOrElse(Array(0: Byte)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    val str = assoc.hash.map(_.base58).getOrElse("")
    jsonBase() ++ Json.obj(
      "version"         -> version,
      "party"           -> assoc.party.stringRepr,
      "associationType" -> assoc.assocType,
      "hash"            -> str,
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

}

case class IssueAssociationTransaction private (override val version: Byte,
                                                override val chainId: Byte,
                                                override val sender: PublicKeyAccount,
                                                override val assoc: Assoc,
                                                override val fee: Long,
                                                override val timestamp: Long,
                                                override val proofs: Proofs)
    extends AssociationTransactionBase(version, chainId, sender, assoc, fee, timestamp, proofs) {
  override val actionType                 = ActionType.Issue
  override def builder: TransactionBuilder = IssueAssociationTransaction
}
case class RevokeAssociationTransaction private (override val version: Byte,
                                                 override val chainId: Byte,
                                                 override val sender: PublicKeyAccount,
                                                 override val assoc: Assoc,
                                                 override val fee: Long,
                                                 override val timestamp: Long,
                                                 override val proofs: Proofs)
    extends AssociationTransactionBase(version, chainId, sender, assoc, fee, timestamp, proofs) {
  override val actionType                 = ActionType.Revoke
  override def builder: TransactionBuilder = RevokeAssociationTransaction
}
object AssociationTransaction {

  type SignedCtor[T] = (Byte, PublicKeyAccount, Address, Int, Option[ByteStr], Long, Long, PrivateKeyAccount) => Either[ValidationError, T]
  type CreateCtor[T] = (Byte, PublicKeyAccount, Address, Int, Option[ByteStr], Long, Long, Proofs) => Either[ValidationError, T]

  sealed trait ActionType
  object ActionType {

    case object Issue  extends ActionType
    case object Revoke extends ActionType
  }

  case class Assoc(party: Address, assocType: Int, hash: Option[ByteStr]) {
    lazy val hashStr = hash.map(_.base58).getOrElse("")
  }

  val supportedVersions: Set[Byte] = Set(1)
  val MaxHashLength                = 64
  val StringHashLength             = com.ltonetwork.utils.base58Length(AssociationTransaction.MaxHashLength)

  def networkByte = AddressScheme.current.chainId

  def parseTail[T](version: Byte, bytes: Array[Byte], create: (Byte, Byte, PublicKeyAccount, Assoc, Long, Long, Proofs) => T): Try[T] =
    Try {

      val chainId = bytes(0)
      val p0      = KeyLength
      val sender  = PublicKeyAccount(bytes.slice(1, p0 + 1))
      val txEi = for {
        party <- Address.fromBytes(bytes.slice(p0 + 1, p0 + 1 + Address.AddressLength))
        partyEnd      = p0 + 1 + Address.AddressLength
        assocType     = Ints.fromByteArray(bytes.slice(partyEnd, partyEnd + 4))
        (hashOpt, s0) = Deser.parseOption(bytes, partyEnd + 4)(ByteStr(_))
        s1            = s0
        timestamp     = Longs.fromByteArray(bytes.drop(s1))
        fee     = Longs.fromByteArray(bytes.drop(s1 + 8))
        proofs <- Proofs.fromBytes(bytes.drop(s1 + 16))
        _      <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
        _      <- validate(version, sender, party, hashOpt, fee)
        tx = create(version, chainId, sender, Assoc(party, assocType, hashOpt), fee, timestamp, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def validate(version: Byte, sender: PublicKeyAccount, party: Address, hash: Option[ByteStr], fee: Long): Either[ValidationError, Unit] = {
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (hash.exists(_.arr.size > MaxHashLength)) {
      Left(ValidationError.GenericError("Hash length must be <=" + MaxHashLength + " bytes"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else if (sender.address == party.address) {
      Left(GenericError("Can't associate with oneself"))
    } else {
      Right(())
    }
  }
}

object IssueAssociationTransaction extends TransactionParserFor[IssueAssociationTransaction] with TransactionBuilder.MultipleVersions {
  override def typeId: Byte                 = 16
  override def supportedVersions: Set[Byte] = Set(1: Byte)
  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[IssueAssociationTransaction] =
    AssociationTransaction.parseTail(version, bytes, IssueAssociationTransaction.apply)

  def signed(version: Byte,
             sender: PublicKeyAccount,
             party: Address,
             assocType: Int,
             hash: Option[ByteStr],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    AssociationTransaction.validate(version, sender, party, hash, fee).map { _ =>
      val uns = IssueAssociationTransaction(
        version = version,
        chainId = AssociationTransaction.networkByte,
        sender = sender,
        assoc = Assoc(party, assocType, hash),
        fee = fee,
        timestamp = timestamp,
        proofs = Proofs.empty
      )
      uns.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, uns.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 party: Address,
                 assocType: Int,
                 hash: Option[ByteStr],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(version, sender, party, assocType, hash, fee, timestamp, sender)
  }

  def create(version: Byte,
             sender: PublicKeyAccount,
             party: Address,
             assocType: Int,
             hash: Option[ByteStr],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, IssueAssociationTransaction] =
    AssociationTransaction
      .validate(version, sender, party, hash, fee)
      .map(_ =>
        IssueAssociationTransaction(version, AssociationTransaction.networkByte, sender, Assoc(party, assocType, hash), fee, timestamp, proofs))
}

object RevokeAssociationTransaction extends TransactionParserFor[RevokeAssociationTransaction] with TransactionBuilder.MultipleVersions {
  override def typeId: Byte                 = 17
  override def supportedVersions: Set[Byte] = Set(1: Byte)
  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[RevokeAssociationTransaction] =
    AssociationTransaction.parseTail(version, bytes, RevokeAssociationTransaction.apply)

  def signed(version: Byte,
             sender: PublicKeyAccount,
             party: Address,
             assocType: Int,
             hash: Option[ByteStr],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    AssociationTransaction.validate(version, sender, party, hash, fee).map { _ =>
      val uns = RevokeAssociationTransaction(version,
                                             AssociationTransaction.networkByte,
                                             sender,
                                             Assoc(party, assocType, hash),
                                             fee,
                                             timestamp,
                                             Proofs.empty)
      uns.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, uns.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 party: Address,
                 assocType: Int,
                 hash: Option[ByteStr],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(version, sender, party, assocType, hash, fee, timestamp, sender)
  }
  def create(version: Byte,
             sender: PublicKeyAccount,
             party: Address,
             assocType: Int,
             hash: Option[ByteStr],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, RevokeAssociationTransaction] =
    AssociationTransaction
      .validate(version, sender, party, hash, fee)
      .map(
        _ =>
          RevokeAssociationTransaction(version,
                                       AssociationTransaction.networkByte,
                                       sender,
                                       Assoc(party, assocType, hash),
                                       fee,
                                       timestamp,
                                       proofs))
}
