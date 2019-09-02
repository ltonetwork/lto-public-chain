package com.wavesplatform.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.account.{Address, AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.crypto
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state._
import com.wavesplatform.transaction.AssociationTransaction.Assoc
import com.wavesplatform.transaction.ValidationError.GenericError
import monix.eval.Coeval
import play.api.libs.json._
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

case class AssociationTransaction private (version: Byte,
                                           chainId: Byte,
                                           sender: PublicKeyAccount,
                                           assoc: Assoc,
                                           fee: Long,
                                           timestamp: Long,
                                           proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = AssociationTransaction
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
      "hash"            -> str
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
}

object AssociationTransaction extends TransactionParserFor[AssociationTransaction] with TransactionParser.MultipleVersions {

  case class Assoc(party: Address, assocType: Int, hash: Option[ByteStr]) {
    lazy val hashStr = hash.map(_.base58).getOrElse("")
  }

  override val typeId: Byte                 = 16
  override val supportedVersions: Set[Byte] = Set(1)

  val HashLength = 64

  private def networkByte = AddressScheme.current.chainId

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      import com.wavesplatform.utils._

      val chainId = bytes(0)
      val p0      = KeyLength
      val sender  = PublicKeyAccount(bytes.slice(1, p0 + 1))
      val txEi = for {
        party <- Address.fromBytes(bytes.slice(p0 + 1, p0 + 1 + Address.AddressLength))
        partyEnd      = p0 + 1 + Address.AddressLength
        assocType     = Ints.fromByteArray(bytes.slice(partyEnd, partyEnd + 4))
        (hashOpt, s0) = Deser.parseOption(bytes, partyEnd + 4)(ByteStr(_))
        timestamp     = Longs.fromByteArray(bytes.drop(s0))
        feeAmount     = Longs.fromByteArray(bytes.drop(s0 + 8))
        proofs <- Proofs.fromBytes(bytes.drop(s0 + 16))
        _      <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
        tx     <- create(version, sender, party, assocType, hashOpt, feeAmount, timestamp, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             party: Address,
             assocType: Int,
             hash: Option[ByteStr],
             feeAmount: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (hash.exists(_.arr.size != HashLength)) {
      Left(ValidationError.GenericError("Hash length must be " + HashLength + " bytes"))
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(AssociationTransaction(version, networkByte, sender, Assoc(party, assocType, hash), feeAmount, timestamp, proofs))
    }
  }

  def signed(version: Byte,
             sender: PublicKeyAccount,
             party: Address,
             assocType: Int,
             hash: Option[ByteStr],
             feeAmount: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(version, sender, party, assocType, hash, feeAmount, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 party: Address,
                 assocType: Int,
                 hash: Option[ByteStr],
                 feeAmount: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(version, sender, party, assocType, hash, feeAmount, timestamp, sender)
  }
}
