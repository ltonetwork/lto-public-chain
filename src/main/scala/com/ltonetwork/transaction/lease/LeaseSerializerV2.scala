package com.ltonetwork.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.transaction.{Proofs, TransactionSerializer}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Either, Failure, Success, Try}

object LeaseSerializerV2 extends LeaseSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    Bytes.concat(Array(LeaseTransaction.typeId, tx.version, 0), bytesBase(tx))
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      (for {
        parsed <- parseBase(bytes, 1)
        (sender, recipient, amount, fee, timestamp, end) = parsed
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tx     <- LeaseTransaction.create(version, None, timestamp, sender, fee, recipient, amount, None, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
