package com.ltonetwork.transaction

import com.ltonetwork.state._
import com.ltonetwork.utils.base58Length
import monix.eval.Coeval
import com.ltonetwork.utils.Base58
import com.ltonetwork.serialization.Deser
import com.ltonetwork.transaction.ValidationError.GenericError

import scala.util.Try

case class Proofs private (proofs: Seq[ByteStr]) {
  val bytes: Coeval[Array[Byte]]  = Coeval.evalOnce(Proofs.Version +: Deser.serializeArrays(proofs.map(_.arr)))
  val base58: Coeval[Seq[String]] = Coeval.evalOnce(proofs.map(p => Base58.encode(p.arr)))
  def toSignature: ByteStr        = proofs.headOption.getOrElse(ByteStr.empty)
  override def toString: String   = s"Proofs(${proofs.mkString(", ")})"

  def ++(other: Proofs): Proofs = Proofs(this.proofs ++ other.proofs)
  def +(proof: ByteStr): Proofs = Proofs(this.proofs ++ Seq(proof))
  def +(proof: Array[Byte]): Proofs = Proofs(this.proofs ++ Seq(ByteStr(proof)))
}

object Proofs {
  val Version: Byte           = 1
  val MaxProofs: Int          = 8
  val MaxProofSize: Int       = 64
  val MaxProofStringSize: Int = base58Length(MaxProofSize)

  lazy val empty = new Proofs(Nil)

  def fromSignature(signature: ByteStr): Proofs = new Proofs(Seq(signature))

  def create(proofs: Seq[ByteStr]): Either[ValidationError, Proofs] =
    for {
      _ <- Either.cond(proofs.lengthCompare(MaxProofs) <= 0, (), GenericError(s"Too many proofs, max $MaxProofs proofs"))
      _ <- Either.cond(!proofs.map(_.arr.length).exists(_ > MaxProofSize), (), GenericError(s"Too large proof, must be max $MaxProofSize bytes"))
    } yield Proofs(proofs)

  def fromBytes(ab: Array[Byte]): Either[ValidationError, Proofs] =
    for {
      _    <- Either.cond(ab.headOption contains 1, (), GenericError(s"Proofs version must be 1, actual:${ab.headOption}"))
      arrs <- Try(Deser.parseArrays(ab.tail)).toEither.left.map(er => GenericError(er.toString))
      r    <- create(arrs.map(ByteStr(_)))
    } yield r

  def apply(proof1: ByteStr, proofs: ByteStr*): Proofs = new Proofs(proof1 +: proofs)
  def apply(proof1: Array[Byte]): Proofs               = new Proofs(Seq(ByteStr(proof1)))
  def apply(proofs: Seq[ByteStr]): Proofs              = new Proofs(proofs)

  implicit def toSeq(proofs: Proofs): Seq[ByteStr]     = proofs.proofs
}
