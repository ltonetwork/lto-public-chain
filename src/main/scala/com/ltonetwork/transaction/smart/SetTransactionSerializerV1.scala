package com.ltonetwork.transaction.smart

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.serialization.Deser
import com.ltonetwork.transaction.{Proofs, TransactionSerializer}
import com.ltonetwork.transaction.smart.SetScriptTransaction.create
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

object SetTransactionSerializerV1 extends TransactionSerializer.For[SetScriptTransaction] {
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

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[SetScriptTransaction] =
    Try {
      val chainId = bytes(0)
      val sender  = PublicKeyAccount(bytes.slice(1, KeyLength + 1))
      val (scriptOptEi, scriptEnd) = SetScriptTransaction.parseScript(bytes, KeyLength + 1)
      val fee       = Longs.fromByteArray(bytes.slice(scriptEnd, scriptEnd + 8))
      val timestamp = Longs.fromByteArray(bytes.slice(scriptEnd + 8, scriptEnd + 16))

      (for {
        scriptOpt <- scriptOptEi
        proofs    <- Proofs.fromBytes(bytes.drop(scriptEnd + 16))
        tx        <- create(version, Some(chainId), timestamp, sender, fee, scriptOpt, None, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  override def toJson(tx: SetScriptTransaction): JsObject = {
    jsonBase(
      tx,
      Json.obj("script" -> tx.script.map(_.bytes().base64))
    )
  }
}
