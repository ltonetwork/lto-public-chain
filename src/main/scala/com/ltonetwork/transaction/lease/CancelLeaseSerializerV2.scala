package com.ltonetwork.transaction.lease

import com.google.common.primitives.Bytes
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.lease.CancelLeaseTransaction.create

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object CancelLeaseSerializerV2 extends CancelLeaseSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] =
    Bytes.concat(Array(CancelLeaseTransaction.typeId, tx.version, tx.chainId), bytesBase(tx))

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val chainId                           = buf.getByte
      val (sender, fee, timestamp, leaseId) = parseBase(buf)
      val proofs                            = buf.getProofs

      create(version, Some(chainId), timestamp, sender, fee, leaseId, None, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
