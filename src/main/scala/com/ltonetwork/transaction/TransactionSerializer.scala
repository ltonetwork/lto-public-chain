package com.ltonetwork.transaction

import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.settings.Constants.TransactionNames
import com.ltonetwork.transaction.Transaction.SigProofsSwitch
import com.ltonetwork.utils.Base58
import monix.eval.Coeval
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

import scala.util.control.NoStackTrace
import scala.util.{Failure, Try}

trait TransactionSerializer {
  type TransactionT <: Transaction

  def bodyBytes(tx: TransactionT): Coeval[Array[Byte]]
  def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT]

  def toJson(tx: TransactionT): Coeval[JsObject]

  private def jsonSponsor(sponsor: Option[PublicKeyAccount]): JsObject = {
    if (sponsor.isDefined) Json.obj(
      "sponsor" -> sponsor.get.address,
      "sponsorKeyType" -> sponsor.get.keyType.reference,
      "sponsorPublicKey" -> Base58.encode(sponsor.get.publicKey)
    ) else Json.obj()
  }

  protected def jsonBase(tx: Transaction, txJson: JsObject): JsObject = {
    import tx._
    Json.obj(
      "type"            -> typeId,
      "version"         -> tx.version,
      "id"              -> id().toString,
      "sender"          -> sender.toAddress,
      "senderKeyType"   -> sender.keyType.reference,
      "senderPublicKey" -> sender,
      "fee"             -> fee,
      "timestamp"       -> timestamp,
    ) ++
      jsonSponsor(sponsor) ++
      txJson ++
      (tx match {
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

  case class UnsupportedVersion(typeId: Byte, version: Byte) extends NoStackTrace {
    override val getMessage: String = {
      val typeName = TransactionNames(typeId)
      s"Unsupported version ($version) for {$typeName} transaction"
    }
  }

  abstract case class Unknown[T <: Transaction](typeId: Byte) extends For[T] {
    override type TransactionT = T

    def bodyBytes(tx: TransactionT): Coeval[Array[Byte]] = Coeval.raiseError(UnsupportedVersion(typeId, tx.version))
    def toJson(tx: TransactionT): Coeval[JsObject] = Coeval.raiseError(UnsupportedVersion(typeId, tx.version))

    def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Failure(UnsupportedVersion(typeId, version))
  }
}