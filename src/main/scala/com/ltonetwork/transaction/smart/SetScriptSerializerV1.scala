package com.ltonetwork.transaction.smart

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization.Deser.serializeArray
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.{TransactionSerializer, ValidationError}
import com.ltonetwork.transaction.smart.SetScriptTransaction.create
import com.ltonetwork.transaction.smart.script.{Script, ScriptReader}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object SetScriptSerializerV1 extends TransactionSerializer.For[SetScriptTransaction] {
  override def bodyBytes(tx: SetScriptTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(SetScriptTransaction.typeId, version, chainId),
      sender.publicKey,
      Deser.serializeOption(script)(s => serializeArray(s.bytes().arr)),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp),
    )
  }

  def parseScript(buf: ByteBuffer): Either[ValidationError.ScriptParseError, Option[Script]] = {
    val scriptOptEi: Option[Either[ValidationError.ScriptParseError, Script]] =
      buf.getOptionalByteArray.map(ScriptReader.fromBytes)

    scriptOptEi match {
      case None            => Right(None)
      case Some(Right(sc)) => Right(Some(sc))
      case Some(Left(err)) => Left(err)
    }
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[SetScriptTransaction] =
    Try {
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
