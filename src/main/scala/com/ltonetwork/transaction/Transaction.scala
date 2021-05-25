package com.ltonetwork.transaction

import com.google.common.primitives.Bytes
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.crypto
import com.ltonetwork.state._
import monix.eval.Coeval
import com.ltonetwork.serialization.{BytesSerializable, JsonSerializable}
import play.api.libs.json.JsObject

trait Transaction extends BytesSerializable with JsonSerializable {
  def builder: TransactionBuilder

  val bytes: Coeval[Array[Byte]]
  val bodyBytes: Coeval[Array[Byte]]
  val json: Coeval[JsObject]

  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
  def typeId: Byte = builder.typeId
  def version: Byte
  def sender: PublicKeyAccount
  def fee: Long
  def timestamp: Long
  def sponsor: Option[PublicKeyAccount]
  def proofs: Proofs

  val footerBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(
    sponsor.map(account => Bytes.concat(Array(account.keyType.id), account.publicKey)).getOrElse(Array(0: Byte)),
    proofs.bytes()
  ))

  override def toString: String = json().toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()
}
