package com.ltonetwork.transaction

import com.google.common.primitives.Longs
import com.ltonetwork.account.KeyTypes.keyType
import com.ltonetwork.serialization._
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.transaction.ValidationError.InvalidPublicKey

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object TransactionParser {
  case class HardcodedVersion1(typeId: Byte) {
    def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] = Try {
      if (bytes.length < 1) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")

      val parsedTypeId = bytes.head
      if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
      (1, 1)
    }
  }

  case class OneVersion(typeId: Byte, version: Byte) {
    def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] = Try {
      if (bytes.length < 2) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")

      val Array(parsedTypeId, parsedVersion) = bytes.take(2)
      if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
      if (parsedVersion != version)
        throw new IllegalArgumentException(s"Expected version of transaction: $version, but got '$parsedVersion'")

      (parsedVersion, 2)
    }
  }

  case class MultipleVersions(typeId: Byte, supportedVersions: Set[Byte]) {
    def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] = Try {
      if (bytes.length < 3) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")

      val Array(parsedMark, parsedTypeId, parsedVersion) = bytes.take(3)
      if (parsedMark != 0) throw new IllegalArgumentException(s"Expected the '0' byte, but got '$parsedMark'")
      if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
      if (!supportedVersions.contains(parsedVersion))
        throw new IllegalArgumentException(s"Expected version of transaction ${supportedVersions.mkString(", ")}, but got '$parsedVersion'")

      (parsedVersion, 3)
    }
  }

  def parseSponsor(buf: ByteBuffer): Option[PublicKeyAccount] =
    try { buf.getOptAccount } catch {
      case e: Exception => throw new Exception("Invalid sponsor public key", e)
    }

  def parseFooter(buf: ByteBuffer): (Option[PublicKeyAccount], Proofs) = {
    val sponsor = try { buf.getOptAccount } catch {
      case e: Exception => throw new Exception("Invalid sponsor public key", e)
    }
    val proofs = buf.getProofs

    (sponsor, proofs)
  }

  // Base structure for transactions v3 and up
  def parseBase(buf: ByteBuffer): (Byte, Long, PublicKeyAccount, Long) = {
    val chainId   = buf.getByte
    val timestamp = buf.getLong
    val sender    = try { buf.getAccount } catch {
      case e: Exception => throw new Exception("Invalid sender public key", e)
    }
    val fee       = buf.getLong

    (chainId, timestamp, sender, fee)
  }
}
