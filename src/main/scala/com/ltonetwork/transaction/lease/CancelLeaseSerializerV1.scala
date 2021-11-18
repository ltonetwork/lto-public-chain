package com.ltonetwork.transaction.lease

import com.google.common.primitives.Bytes
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.Proofs
import com.ltonetwork.transaction.lease.CancelLeaseTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object CancelLeaseSerializerV1 extends CancelLeaseSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] =
    Bytes.concat(Array(CancelLeaseTransaction.typeId), bytesBase(tx))

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (sender, fee, timestamp, leaseId) = parseBase(buf)
      val signature                         = buf.getSignature

      create(version, None, timestamp, sender, fee, leaseId, None, Proofs.fromSignature(signature))
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
