package com.ltonetwork.transaction

import com.google.common.primitives.Longs
import com.ltonetwork.account.KeyTypes.keyType
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.transaction.ValidationError.InvalidPublicKey

import scala.util.Try

object TransactionParser {
  private def longLength = 8

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

  private def parsePublicKeyAccount(bytes: Array[Byte], start: Int): Option[PublicKeyAccount] = {
    val keyTypeId = bytes(start)

    keyType(keyTypeId) map {
      kt => PublicKeyAccount(kt, bytes.slice(1, 1 + kt.length))
    }
  }

  def parseSponsor(bytes: Array[Byte], start: Int): Either[ValidationError, Option[PublicKeyAccount]] = {
    val keyTypeId = bytes(start)

    if (keyTypeId == 0)
      Right(None)
    else
      parsePublicKeyAccount(bytes, start)
        .toRight(InvalidPublicKey("Invalid sponsor key type"))
        .map(sponsor => Some(sponsor))
  }

  // Base structure for transactions v3 and up
  def parseBase(bytes: Array[Byte]): Either[ValidationError, (Byte, Long, PublicKeyAccount, Long, Int)] = {
    val chainId = bytes.head
    val timestamp = Longs.fromByteArray(bytes.slice(1, 1 + longLength))

    parsePublicKeyAccount(bytes, 1 + longLength)
      .toRight(InvalidPublicKey("Invalid sender key type"))
      .map(sender => {
        val s1 = 1 + longLength + 2 + sender.keyType.length
        val fee = Longs.fromByteArray(bytes.slice(s1, s1 + longLength))

        (chainId, timestamp, sender, fee, s1 + longLength)
      })
  }
}
