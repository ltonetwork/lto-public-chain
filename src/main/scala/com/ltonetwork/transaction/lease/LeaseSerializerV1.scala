package com.ltonetwork.transaction.lease

import com.google.common.primitives.Bytes
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.Proofs
import monix.eval.Coeval
import scorex.crypto.signatures.Curve25519.SignatureLength

import scala.util.{Failure, Success, Try}

object LeaseSerializerV1 extends LeaseSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    Bytes.concat(Array(LeaseTransaction.typeId), bytesBase(tx))
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      (for {
        parsed <- parseBase(bytes, 0)
        (sender, recipient, amount, fee, timestamp, end) = parsed
        signature                                        = ByteStr(bytes.slice(end, end + SignatureLength))
        tx <- LeaseTransaction.create(version, None, timestamp, sender, fee, recipient, amount, None, Proofs.fromSignature(signature))
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
