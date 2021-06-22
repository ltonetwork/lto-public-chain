package com.ltonetwork.transaction.lease

import com.google.common.primitives.Bytes
import com.ltonetwork.transaction.Proofs
import com.ltonetwork.transaction.lease.CancelLeaseTransaction.create
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

object CancelLeaseSerializerV2 extends CancelLeaseSerializerLegacy {
  override def bodyBytes(tx: TransactionT): Array[Byte] =
    Bytes.concat(Array(CancelLeaseTransaction.typeId, tx.version, tx.chainId), bytesBase(tx))

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val chainId                                = bytes(0)
      val (sender, fee, timestamp, leaseId, end) = parseBase(bytes, 1)
      (for {
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tx     <- create(version, Some(chainId), timestamp, sender, fee, leaseId, None, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
