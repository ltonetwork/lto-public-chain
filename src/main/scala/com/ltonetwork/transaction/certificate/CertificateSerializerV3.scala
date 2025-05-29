package com.ltonetwork.transaction.certificate

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.certificate.CertificateTransaction.create
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object CertificateSerializerV3 extends TransactionSerializer.For[CertificateTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: CertificateTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(CertificateTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Deser.serializeArray(certificate) // includes length prefix
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[CertificateTransaction] =
    Try {
      val buf = ByteBuffer.wrap(bytes)

      val (chainId, timestamp, sender, fee) = parseBase(buf)
      val certificate                       = buf.getByteArrayWithLength
      val (sponsor, proofs)                 = parseFooter(buf)

      create(version, Some(chainId), timestamp, sender, fee, certificate, sponsor, proofs)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
