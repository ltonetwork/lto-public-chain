package com.ltonetwork.transaction

import com.ltonetwork.settings.Constants.TransactionNames
import play.api.libs.json.JsObject

import scala.util.control.NoStackTrace
import scala.util.{Failure, Try}

trait TransactionSerializer {
  type TransactionT <: Transaction

  def bodyBytes(tx: TransactionT): Array[Byte]
  def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT]
}

object TransactionSerializer {
  abstract class For[T <: Transaction] extends TransactionSerializer {
    override type TransactionT = T
  }

  case class UnsupportedVersion(typeId: Byte, version: Byte) extends NoStackTrace {
    override val getMessage: String = {
      val typeName = TransactionNames(typeId)
      s"Unsupported version ($version) for ${typeName} transaction"
    }
  }

  abstract case class Unknown[T <: Transaction](typeId: Byte) extends For[T] {
    override type TransactionT = T

    def bodyBytes(tx: TransactionT): Array[Byte] = throw UnsupportedVersion(typeId, tx.version)
    def toJson(tx: TransactionT): JsObject       = throw UnsupportedVersion(typeId, tx.version)

    def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Failure(UnsupportedVersion(typeId, version))
  }
}
