package com.ltonetwork.transaction.lease

import com.google.common.primitives.Bytes
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.lease.LeaseTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object LeaseSerializerV2 extends LeaseSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] =
    Bytes.concat(Array(LeaseTransaction.typeId, tx.version, 0), bytesBase(tx))

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      buf.skip(1) // First byte is always a zero

      val (sender, recipient, amount, fee, timestamp) = parseBase(buf)
      val proofs                                      = buf.getProofs

      create(version, None, timestamp, sender, fee, recipient, amount, None, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
