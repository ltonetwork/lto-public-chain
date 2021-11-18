package com.ltonetwork.transaction

import com.google.common.primitives.Bytes
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.utils.Base58
import monix.eval.Coeval
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

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
  def feeSponsor: Option[Address]

  protected def prefixByte: Coeval[Array[Byte]] = Coeval.evalOnce(Array(0: Byte))
  private def sponsorBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (version >= 3) sponsor.fold(Array(0: Byte))(Deser.serializeAccount)
    else Array.emptyByteArray
  )
  protected def footerBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(sponsorBytes(), proofs.bytes()))
  val bytes: Coeval[Array[Byte]]                 = Coeval.evalOnce(Bytes.concat(prefixByte(), bodyBytes(), footerBytes()))

  protected def jsonBase: Transaction.TxJson = Transaction.TxJson(this)

  override def toString: String = json().toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()

  def withKnownFeeSponsor(sponsor: Option[Address]): this.type = this.copy(feeSponsor = sponsor)
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

  case class TxJson(tx: Transaction) {
    import tx._

    private def headerJson: JsObject = {
      Json.obj(
        "type"    -> typeId,
        "version" -> version,
        "id"      -> id().toString
      )
    }

    private def senderJson: JsObject = {
      Json.obj(
        "sender"          -> sender.address,
        "senderKeyType"   -> sender.keyType.reference,
        "senderPublicKey" -> Base58.encode(sender.publicKey),
      )
    }

    private def sponsorJson: JsObject =
      sponsor
        .map(
          acc =>
            Json.obj(
              "sponsor"          -> acc.address,
              "sponsorKeyType"   -> acc.keyType.reference,
              "sponsorPublicKey" -> Base58.encode(acc.publicKey)
          )
        )
        .getOrElse(Json.obj())

    private def proofsJson: JsObject = tx match {
      case s: SigProofsSwitch if s.usesLegacySignature => Json.obj("signature" -> s.signature.toString)
      case _ if tx.proofs.proofs.nonEmpty              => Json.obj("proofs" -> JsArray(tx.proofs.proofs.map(p => JsString(p.toString))))
      case _                                           => Json.obj()
    }

    //noinspection ScalaStyle
    def ++(txSpecificJson: JsObject): JsObject =
      headerJson ++ senderJson ++ sponsorJson ++
        Json.obj("fee" -> fee, "timestamp" -> timestamp) ++ txSpecificJson ++ proofsJson
  }
}
