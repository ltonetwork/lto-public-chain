package com.ltonetwork.transaction

import com.ltonetwork.transaction.ValidationError.UnsupportedVersion
import com.ltonetwork.utils.Base58
import monix.eval.Coeval
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

import scala.util.{Failure, Try}

trait TransactionSerializer {
  type TransactionT <: Transaction

  def bodyBytes(tx: TransactionT): Coeval[Array[Byte]]
  def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT]

  def json(tx: TransactionT): Coeval[JsObject]

  protected def jsonBase(tx: Transaction, txJson: JsObject): JsObject = {
    import tx._
    Json.obj(
      "type"            -> typeId,
      "version"         -> tx.version,
      "id"              -> id().toString,
      "sender"          -> sender.toAddress,
      "senderKeyType"   -> sender.keyType.id,
      "senderPublicKey" -> sender,
      "fee"             -> fee,
      "timestamp"       -> timestamp,
    ) ++ {
      if (sponsor.isDefined) Json.obj(
        "sponsor" -> sponsor.get.address,
        "sponsorKeyType" -> sponsor.get.keyType.id,
        "sponsorPublicKey" -> Base58.encode(sponsor.get.publicKey)
      )
      else Json.obj()
    } ++ txJson ++ (tx match {
      case s: SigProofsSwitch if s.usesLegacySignature => Json.obj("signature" -> s.signature.toString)
      case _ if proofs.proofs.nonEmpty                 => Json.obj("proofs" -> JsArray(proofs.proofs.map(p => JsString(p.toString))))
      case _                                           => Json.obj()
    })
  }
}

object TransactionSerializer {
  abstract class For[T <: Transaction] extends TransactionSerializer {
    override type TransactionT = T
  }

  abstract class Unknown[T <: Transaction] extends For[T] {
    override type TransactionT = T

    def bodyBytes(tx: TransactionT): Coeval[Array[Byte]] = Coeval.raiseError(UnsupportedVersion(tx.version))
    def json(tx: TransactionT): Coeval[JsObject] = Coeval.raiseError(UnsupportedVersion(tx.version))

    def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Failure(UnsupportedVersion(version))
  }
}