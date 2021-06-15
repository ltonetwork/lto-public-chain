package com.ltonetwork.account

import com.ltonetwork.account.KeyTypes._
import com.ltonetwork.crypto
import com.ltonetwork.utils.Base58
import com.ltonetwork.transaction.ValidationError.GenericError

import scala.util.{Failure, Success}

sealed trait PrivateKeyAccount extends PublicKeyAccount {
  def seed: Array[Byte]
  def privateKey: Array[Byte]
}

object PrivateKeyAccount {

  private case class PrivateKeyAccountImpl(seed: Array[Byte], keyType: KeyType, privateKey: Array[Byte], publicKey: Array[Byte]) extends PrivateKeyAccount

  def apply(seed: Array[Byte]): PrivateKeyAccount = {
    val pair = crypto.createKeyPair(seed)
    PrivateKeyAccountImpl(seed, ED25519, pair._1, pair._2)
  }

  def fromSeed(s: String): Either[GenericError, PrivateKeyAccount] = Base58.decode(s) match {
    case Success(x) => Right(PrivateKeyAccount(x))
    case Failure(e) => Left(GenericError(s"Unable to get a private key from the seed '$s': ${e.getMessage}"))
  }

}
