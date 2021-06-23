package com.ltonetwork.transaction.lease

import com.google.common.primitives.Bytes
import com.ltonetwork.crypto
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.Proofs
import com.ltonetwork.transaction.lease.CancelLeaseTransaction.create
import monix.eval.Coeval
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}

import scala.util.{Failure, Success, Try}

object CancelLeaseSerializerV1 extends CancelLeaseSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] =
    Bytes.concat(Array(CancelLeaseTransaction.typeId), bytesBase(tx))

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val (sender, fee, timestamp, leaseId, end) = parseBase(bytes, 0)
      val signature                              = ByteStr(bytes.slice(end, KeyLength + 16 + crypto.DigestSize + SignatureLength))
      create(version, None, timestamp, sender, fee, leaseId, None, Proofs.fromSignature(signature))
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
