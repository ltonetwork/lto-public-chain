package com.ltonetwork

import com.emstlk.nacl4s.crypto.SigningKeyPair
import com.emstlk.nacl4s.{SigningKey, VerifyKey}
import com.ltonetwork.account.{KeyType, KeyTypes, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.seasalt.sign.ECDSA
import scorex.crypto.hash.{Blake2b256, Sha256}

import scala.util.Try

package object crypto {
  val SignatureLength: Int = 64 //Curve25519
  val KeyLength: Int       = 32 //Curve25519
  val DigestLength: Int    = 32

  def fastHash(m: Array[Byte]): Array[Byte]   = Blake2b256.hash(m)
  def fastHash(s: String): Array[Byte]        = fastHash(s.getBytes())
  def secureHash(m: Array[Byte]): Array[Byte] = Sha256.hash(Blake2b256.hash(m))
  def secureHash(s: String): Array[Byte]      = secureHash(s.getBytes())

  def sign(account: PrivateKeyAccount, message: Array[Byte]): Array[Byte] =
    sign(account.privateKey, account.keyType, message)

  def sign(privateKey: Array[Byte], message: Array[Byte]): Array[Byte] =
    sign(privateKey, KeyTypes.ED25519, message)

  def sign(privateKey: Array[Byte], keyType: KeyType, message: Array[Byte]): Array[Byte] = keyType match {
    case KeyTypes.ED25519 =>
      SigningKey(privateKey).sign(message)
    case KeyTypes.SECP256K1 =>
      new ECDSA("secp256k1").signDetached(message, privateKey).getBytes
    case KeyTypes.SECP256R1 =>
      new ECDSA("secp256r1").signDetached(message, privateKey).getBytes
    case _ =>
      throw new IllegalArgumentException("Unknown key type")
  }

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean =
    verify(signature, message, publicKey, KeyTypes.ED25519)

  def verify(signature: Array[Byte], message: Array[Byte], account: PublicKeyAccount): Boolean =
    verify(signature, message, account.publicKey, account.keyType)

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte], keyType: KeyType): Boolean = keyType match {
    case KeyTypes.ED25519 =>
      Try(VerifyKey(publicKey).verify(message, signature)).fold(_ => false, _ => true)
    case KeyTypes.SECP256K1 =>
      new ECDSA("secp256k1").verify(message, signature, publicKey)
    case KeyTypes.SECP256R1 =>
      new ECDSA("secp256r1").verify(message, signature, publicKey)
    case _ =>
      throw new IllegalArgumentException("Unknown key type")
  }

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) =
    createKeyPair(seed, KeyTypes.ED25519)

  def createKeyPair(seed: Array[Byte], keyType: KeyType): (Array[Byte], Array[Byte]) = keyType match {
    case KeyTypes.ED25519 =>
      //val hash = new Hasher("SHA-256").hash(seed).getBytes
      //val kp   = SigningKeyPair(hash)
      val kp = SigningKeyPair(Sha256.hash(seed))
      (kp.privateKey, kp.publicKey)
    case KeyTypes.SECP256K1 =>
      val kp = new ECDSA("secp256k1").keyPairFromSeed(seed)
      (kp.getPrivateKey.getBytes, kp.getPublicKey.getBytes)
    case KeyTypes.SECP256R1 =>
      val kp = new ECDSA("secp256r1").keyPairFromSeed(seed)
      (kp.getPrivateKey.getBytes, kp.getPublicKey.getBytes)
    case _ =>
      throw new IllegalArgumentException("Unknown key type")
  }
}
