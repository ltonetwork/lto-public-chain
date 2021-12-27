package com.ltonetwork.account

import com.ltonetwork.account.KeyTypes._
import com.ltonetwork.crypto
import com.ltonetwork.transaction.ValidationError.InvalidAddress
import com.ltonetwork.utils.{Base58, base58Length}

trait PublicKeyAccount {
  def keyType: KeyType
  def publicKey: Array[Byte]

  override def equals(b: Any): Boolean = b match {
    case a: PublicKeyAccount => publicKey.sameElements(a.publicKey)
    case _                   => false
  }

  override def hashCode(): Int = publicKey.hashCode()

  override lazy val toString: String = this.toAddress.address

  def verify(signature: Array[Byte], message: Array[Byte]): Boolean = crypto.verify(signature, message, this)
}

object PublicKeyAccount {
  private case class PublicKeyAccountImpl(keyType: KeyType, publicKey: Array[Byte]) extends PublicKeyAccount

  def apply(publicKey: Array[Byte]): PublicKeyAccount                   = PublicKeyAccountImpl(ED25519, publicKey)
  def apply(keyType: KeyType, publicKey: Array[Byte]): PublicKeyAccount = PublicKeyAccountImpl(keyType, publicKey)

  implicit def toAddress(publicKeyAccount: PublicKeyAccount): Address = Address.fromPublicKey(publicKeyAccount.publicKey)

  implicit class PublicKeyAccountExt(pk: PublicKeyAccount) {
    def toAddress: Address = PublicKeyAccount.toAddress(pk)
  }

  object Dummy extends PublicKeyAccount {
    def keyType: KeyType       = ED25519
    def publicKey: Array[Byte] = Array[Byte](0)
  }

  def fromBase58String(keyType: KeyType, s: String): Either[InvalidAddress, PublicKeyAccount] =
    (for {
      _     <- Either.cond(s.length <= base58Length(keyType.length), (), "Bad public key string length")
      bytes <- Base58.decode(s).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PublicKeyAccount(keyType, bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))

  def fromBase58String(s: String): Either[InvalidAddress, PublicKeyAccount] =
    fromBase58String(ED25519, s)
}
