package com.ltonetwork.transaction.association

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.association.RevokeAssociationTransaction.create
import com.ltonetwork.transaction.{Proofs, TransactionSerializer}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

object RevokeAssociationSerializerV1 extends TransactionSerializer.For[RevokeAssociationTransaction] {
  override def bodyBytes(tx: RevokeAssociationTransaction): Coeval[Array[Byte]] = Coeval.evalOnce {
    import tx._

    Bytes.concat(
      Array(RevokeAssociationTransaction.typeId, version, chainId),
      sender.publicKey,
      recipient.bytes.arr,
      Ints.toByteArray(assocType),
      hash.map(a => (1: Byte) +: Deser.serializeArray(a.arr)).getOrElse(Array(0: Byte)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  override protected def parseBytes(version: Byte, bytes: Array[Byte]): Try[RevokeAssociationTransaction] =
    Try {
      val chainId = bytes(0)
      (for {
        parsed <- AssociationSerializerV1.parse(version, bytes)
        (version, timestamp, sender, fee, assocType, recipient, hashOpt, proofs) = parsed
        tx     <- create(version, Some(chainId), timestamp, sender, fee, assocType, recipient, hashOpt, None, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  override def toJson(tx: RevokeAssociationTransaction): Coeval[JsObject] = Coeval.evalOnce {
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
