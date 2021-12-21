package com.ltonetwork.account

import com.ltonetwork.account.KeyTypes._
import com.ltonetwork.crypto
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.utils.Base58

import scala.util.{Failure, Success}

sealed trait PrivateKeyAccount extends PublicKeyAccount {
  def seed: Array[Byte]
  def privateKey: Array[Byte]

  def sign(message: Array[Byte]): Array[Byte] = crypto.sign(this, message)
}

object PrivateKeyAccount {

  private case class PrivateKeyAccountImpl(seed: Array[Byte], keyType: KeyType, privateKey: Array[Byte], publicKey: Array[Byte])
      extends PrivateKeyAccount

  def apply(seed: Array[Byte], keyType: KeyType): PrivateKeyAccount = {
    val pair = crypto.createKeyPair(seed, keyType)
    PrivateKeyAccountImpl(seed, keyType, pair._1, pair._2)
  }

  def apply(seed: Array[Byte]): PrivateKeyAccount = {
    apply(seed, ED25519)
  }

  def fromSeed(s: String, keyType: KeyType): Either[GenericError, PrivateKeyAccount] = Base58.decode(s) match {
    case Success(x) => Right(PrivateKeyAccount(x, keyType))
    case Failure(e) => Left(GenericError(s"Unable to get a private key from the seed '$s': ${e.getMessage}"))
  }

  def fromSeed(s: String): Either[GenericError, PrivateKeyAccount] = {
    fromSeed(s, ED25519)
  }

}
