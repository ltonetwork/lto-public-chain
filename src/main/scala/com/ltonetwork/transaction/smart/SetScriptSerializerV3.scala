package com.ltonetwork.transaction.smart

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.{TransactionParser, TransactionSerializer, ValidationError}
import com.ltonetwork.transaction.smart.SetScriptTransaction.create
import com.ltonetwork.transaction.smart.script.{Script, ScriptReader}

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object SetScriptSerializerV3 extends TransactionSerializer.For[SetScriptTransaction] {
  import TransactionParser._

  override def bodyBytes(tx: SetScriptTransaction): Array[Byte] = {
    import tx._

    Bytes.concat(
      Array(SetScriptTransaction.typeId, version, chainId),
      Longs.toByteArray(timestamp),
      Deser.serializeAccount(sender),
      Longs.toByteArray(fee),
      Deser.serializeArray(script.fold(Array.emptyByteArray)(s => s.bytes().arr))
    )
  }

  def parseScript(buf: ByteBuffer): Either[ValidationError.ScriptParseError, Option[Script]] = {
    val scriptBytes = buf.getByteArrayWithLength

    if (scriptBytes.isEmpty) Right(None)
    else ScriptReader.fromBytes(scriptBytes).map(Some(_))
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[SetScriptTransaction] = Try {
    val buf = ByteBuffer.wrap(bytes)

    val (chainId, timestamp, sender, fee) = parseBase(buf)
    val scriptOptEi = parseScript(buf)
    val (sponsor, proofs) = parseFooter(buf)

    (for {
      scriptOpt <- scriptOptEi
      tx        <- create(version, Some(chainId), timestamp, sender, fee, scriptOpt, sponsor, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
