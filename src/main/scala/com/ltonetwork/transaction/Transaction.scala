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

  val bodyBytes: Coeval[Array[Byte]]
  val json: Coeval[JsObject]

  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
  def typeId: Byte        = builder.typeId
  def chainId: Byte
  def version: Byte
  def sender: PublicKeyAccount
  def fee: Long
  def timestamp: Long
  def sponsor: Option[PublicKeyAccount]
  def proofs: Proofs

  protected def prefixByte: Coeval[Array[Byte]] = Coeval.evalOnce(Array(0: Byte))
  private def sponsorBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (version >= 3) sponsor.map(account => Bytes.concat(Array(account.keyType.id), account.publicKey)).getOrElse(Array(0: Byte))
    else Array.emptyByteArray
  )
  protected def footerBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(sponsorBytes(), proofs.bytes()))
  val bytes: Coeval[Array[Byte]]                 = Coeval.evalOnce(Bytes.concat(prefixByte(), bodyBytes(), footerBytes()))

  override def toString: String = json().toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()
}

object Transaction {
  trait HardcodedV1 extends Transaction {
    override protected def prefixByte: Coeval[Array[Byte]] = Coeval.evalOnce(
      if (this.version == 1) Array.emptyByteArray
      else Array(0: Byte)
    )
  }

  trait SigProofsSwitch extends Transaction {
    def usesLegacySignature: Boolean = version == 1
    def signature: ByteStr           = proofs.toSignature

    override protected def footerBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
      if (this.version == 1) proofs.toSignature.arr
      else super.footerBytes()
    )
  }
}
