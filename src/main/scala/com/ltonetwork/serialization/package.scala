package com.ltonetwork

import com.google.common.primitives.Shorts
import com.ltonetwork.account.KeyTypes.keyType
import com.ltonetwork.account.{Address, PublicKeyAccount}
import com.ltonetwork.state._
import com.ltonetwork.transaction.Proofs
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}

import java.nio.ByteBuffer
import scala.util.{Failure, Success}

package object serialization {
  implicit class ByteBufferOps(private val buf: ByteBuffer) extends AnyVal {
    def getByteArrayWithLength: Array[Byte] = {
      val prefix = buf.getShort
      require(prefix >= 0, "negative array length")
      if (prefix > 0) getByteArray(prefix) else Array.emptyByteArray
    }

    def getAddress: Address = {
      Address.fromBytes(getByteArray(Address.AddressLength)).explicitGet()
    }

    def skip(i: Int): Unit = buf.position(buf.position() + i)

    // More explicit name
    def getByte: Byte = buf.get()

    def getBoolean: Boolean = buf.get match {
      case 0 => false
      case 1 => true
      case b => throw new IllegalArgumentException(s"Invalid boolean value: $b")
    }

    def getByteArray(size: Int): Array[Byte] = {
      val result = new Array[Byte](size)
      buf.get(result)
      result
    }

    def getShortArray(size: Int): Array[Short] = {
      val result = new Array[Short](size)
      buf.asShortBuffer().get(result)
      buf.position(buf.position() + Shorts.BYTES * size)
      result
    }

    def getArrays: Seq[Array[Byte]] = {
      val length = buf.getShort

      (0 until length).foldLeft(Seq.empty[Array[Byte]]) {
        case (acc, _) => acc :+ getByteArrayWithLength
      }
    }

    def getOptionalByteArray: Option[Array[Byte]] =
      if (buf.get == 1) {
        val length = buf.getShort
        Some(buf.getByteArray(length))
      } else None

    def getSignature: ByteStr = ByteStr(getByteArray(SignatureLength))

    def getPublicKey: PublicKeyAccount = PublicKeyAccount(getByteArray(KeyLength))

    private def getAccountWithKeyType(keyTypeId: Byte, buf: ByteBuffer): PublicKeyAccount =
      keyType(keyTypeId) match {
        case Failure(exception) => throw exception
        case Success(kt) => PublicKeyAccount(kt, buf.getByteArray(kt.length))
      }

    def getAccount: PublicKeyAccount = getAccountWithKeyType(buf.get, buf)
    def getOptAccount: Option[PublicKeyAccount] = {
      val kt = buf.get
      if (kt == 0) None else Some(getAccountWithKeyType(kt, buf))
    }

    def getProofs: Proofs = Proofs.fromBytes(buf.getByteArray(buf.remaining())).explicitGet()
  }
}
