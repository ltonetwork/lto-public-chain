package com.ltonetwork.lang

import com.emstlk.nacl4s.VerifyKey
import com.ltonetwork.lang.v1.BaseGlobal
import com.ltonetwork.utils.{Base58, Base64}
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}

import scala.util.Try

object Global extends BaseGlobal {
  def base58Encode(input: Array[Byte]): Either[String, String] =
    if (input.length > MaxBase58Bytes) Left(s"base58Encode input exceeds $MaxBase58Bytes")
    else Right(Base58.encode(input))

  def base58Decode(input: String, limit: Int): Either[String, Array[Byte]] =
    if (input.length > limit) Left(s"base58Decode input exceeds $limit")
    else Base58.decode(input).toEither.left.map(_ => "can't parse Base58 string")

  def base64Encode(input: Array[Byte]): Either[String, String] =
    Either.cond(input.length <= MaxBase64Bytes, Base64.encode(input), s"base64Encode input exceeds $MaxBase64Bytes")

  def base64Decode(input: String, limit: Int): Either[String, Array[Byte]] =
    for {
      _      <- Either.cond(input.length <= limit, (), s"base64Decode input exceeds $limit")
      result <- Base64.decode(input).toEither.left.map(_ => "can't parse Base64 string")
    } yield result

  def signatureVerify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean =
    Try(VerifyKey(pub).verify(message, sig)).fold(_ => false, _ => true)

  def keccak256(message: Array[Byte]): Array[Byte]  = Keccak256.hash(message)
  def blake2b256(message: Array[Byte]): Array[Byte] = Blake2b256.hash(message)
  def sha256(message: Array[Byte]): Array[Byte]     = Sha256.hash(message)
}
