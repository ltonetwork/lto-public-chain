package com.ltonetwork.transaction.smart

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.TransactionSerializer
import com.ltonetwork.transaction.smart.SetScriptTransaction.{create, parseScript}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object SetScriptSerializerV1 extends TransactionSerializer.For[SetScriptTransaction] {
  override def bodyBytes(tx: SetScriptTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(SetScriptTransaction.typeId, version, chainId),
      sender.publicKey,
      Deser.serializeOption(script)(s => s.bytes().arr),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp),
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[SetScriptTransaction] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val chainId     = buf.getByte
    val sender      = buf.getPublicKey
    val scriptOptEi = parseScript(buf)
    val fee         = buf.getLong
    val timestamp   = buf.getLong
    val proofs      = buf.getProofs

    (for {
      scriptOpt <- scriptOptEi
      tx        <- create(version, Some(chainId), timestamp, sender, fee, scriptOpt, None, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
