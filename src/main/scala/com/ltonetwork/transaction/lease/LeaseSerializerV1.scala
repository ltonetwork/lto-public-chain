package com.ltonetwork.transaction.lease

import com.google.common.primitives.Bytes
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.Proofs
import com.ltonetwork.transaction.lease.LeaseTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object LeaseSerializerV1 extends LeaseSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] =
    Bytes.concat(Array(LeaseTransaction.typeId), bytesBase(tx))

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (sender, recipient, amount, fee, timestamp) = parseBase(buf)
      val signature                                   = buf.getSignature

      create(version, None, timestamp, sender, fee, recipient, amount, None, Proofs.fromSignature(signature))
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
