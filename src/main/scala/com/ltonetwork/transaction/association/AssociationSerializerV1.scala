package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.{Address, AddressOrAlias, PublicKeyAccount}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.{Proofs, TransactionSerializer, ValidationError}
import com.ltonetwork.transaction.association.AssociationTransaction.create
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

object AssociationSerializerV1 extends TransactionSerializer.For[AssociationTransaction] {
  override def bodyBytes(tx: AssociationTransaction): Coeval[Array[Byte]] = Coeval.evalOnce {
    import tx._

    Bytes.concat(
      Array(AssociationTransaction.typeId, version, chainId),
      sender.publicKey,
      recipient.bytes.arr,
      Ints.toByteArray(assocType),
      hash.map(a => (1: Byte) +: Deser.serializeArray(a.arr)).getOrElse(Array(0: Byte)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  override protected def parseBytes(version: Byte, bytes: Array[Byte]): Try[AssociationTransaction] =
    Try {
      val chainId = bytes(0)
      (for {
        parsed <- parse(version, bytes)
        (version, timestamp, sender, fee, assocType, recipient, hashOpt, proofs) = parsed
        tx     <- create(version, timestamp, sender, fee, assocType, recipient, None, hashOpt, None, proofs)
        tx.chainId = chainId
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def parse(version: Byte, bytes: Array[Byte]): Either[ValidationError, (Byte, Long, PublicKeyAccount, Long, Int, Address, Option[ByteStr], Proofs)] = {
    val p0      = KeyLength
    val sender  = PublicKeyAccount(bytes.slice(1, p0 + 1))

    for {
      recipient <- Address.fromBytes(bytes.slice(p0 + 1, p0 + 1 + Address.AddressLength))
      recipientEnd  = p0 + 1 + Address.AddressLength
      assocType     = Ints.fromByteArray(bytes.slice(recipientEnd, recipientEnd + 4))
      (hashOpt, s0) = Deser.parseOption(bytes, recipientEnd + 4)(ByteStr(_))
      s1            = s0
      timestamp     = Longs.fromByteArray(bytes.drop(s1))
      fee           = Longs.fromByteArray(bytes.drop(s1 + 8))
      proofs <- Proofs.fromBytes(bytes.drop(s1 + 16))
      result = (version, timestamp, sender, fee, assocType, recipient, hashOpt, proofs)
    } yield result
  }

  override def toJson(tx: AssociationTransaction): Coeval[JsObject] = Coeval.evalOnce {
    jsonBase(
      tx,
      Json.obj(
        "version"         -> tx.version,
        "associationType" -> tx.assocType,
        "party"           -> tx.recipient.stringRepr,
      )
        ++ tx.hash.map(hash => Json.obj("hash" -> hash.base58)).getOrElse(Json.obj())
    )
  }
}
