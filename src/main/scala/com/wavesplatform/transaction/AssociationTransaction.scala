package com.wavesplatform.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.crypto
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state._
import monix.eval.Coeval
import play.api.libs.json._
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.Try

case class AssociationTransaction private (version: Byte, sender: PublicKeyAccount, party: Address, assocType: Int, hash: Option[ByteStr], fee: Long, timestamp: Long, proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = AnchorTransaction
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      party.bytes.arr,
      hash.map(a => (1: Byte) +: Deser.serializeArray(a.arr)).getOrElse(Array(0: Byte)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version" -> version,
      "party" -> party.stringRepr,
      "associationType" -> assocType,
      "hash" -> hash
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
}

object AssociationTransaction extends TransactionParserFor[AssociationTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 16
  override val supportedVersions: Set[Byte] = Set(1)

  val HashLength      = 64

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val p0     = KeyLength
      val sender = PublicKeyAccount(bytes.slice(0, p0))
      ???
//
//      val txEi = for {
//        r <- Try(Deser.parseArraysPos(bytes.drop(p0))).toEither.left.map(x => GenericError(x.toString))
//        arrays    = r._1
//        pos       = r._2
//        timestamp = Longs.fromByteArray(bytes.drop(p0 + pos))
//        feeAmount = Longs.fromByteArray(bytes.drop(p0 + pos + 8))
//
//        proofs <- Proofs.fromBytes(bytes.drop(p0 + pos + 16))
//        tx     <- create(version, sender, arrays.map(ByteStr(_)).toList, feeAmount, timestamp, proofs)
//      } yield tx
//      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             party: Address, assocType: Int, hash: Option[ByteStr],
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
      Right( AssociationTransaction(version, sender, party,assocType,hash, feeAmount, timestamp, proofs))
    }
  }

  def signed(version: Byte,
             sender: PublicKeyAccount,
             party: Address, assocType: Int, hash: Option[ByteStr],
             feeAmount: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(version, sender,   party, assocType, hash, feeAmount, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 party: Address, assocType: Int, hash: Option[ByteStr],
                 feeAmount: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(version, sender, party, assocType, hash, feeAmount, timestamp, sender)
  }
}


