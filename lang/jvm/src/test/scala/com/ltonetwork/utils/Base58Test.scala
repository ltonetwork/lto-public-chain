package com.ltonetwork.utils

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scorex.crypto.encode.{Base58 => scorexBase58}

class Base58Test extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  private val Base58Chars  = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val IllegalChars = "+/=-_0lIO"

  val base58Gen: Gen[String] =
    for {
      length <- Gen.chooseNum(20, 100)
      chars  <- Gen.listOfN(length, Gen.oneOf(Base58Chars))
    } yield chars.mkString

  val nonBase58Gen: Gen[String] =
    for {
      length <- Gen.chooseNum(20, 100)
      chars <- Gen
        .listOfN(length, Gen.oneOf(Base58Chars ++ IllegalChars))
        .filter(_.toSet.intersect(IllegalChars.toSet).nonEmpty)
    } yield chars.mkString

  property("works the same as scorex implementation") {
    forAll(base58Gen) { s =>
      val bytes       = Base58.decode(s).get
      val scorexBytes = scorexBase58.decode(s).get
      bytes.sameElements(scorexBytes) shouldBe true

      val str       = Base58.encode(bytes)
      val scorexStr = scorexBase58.encode(bytes)
      str shouldBe scorexStr
    }
  }

  property("handles empty sequences") {
    Base58.encode(Array.emptyByteArray) shouldBe ""
    val d = Base58.decode("")
    d.isSuccess shouldBe true
    d.get.length shouldBe 0
  }

  property("decoding fails on illegal characters") {
    forAll(nonBase58Gen) { s =>
      Base58.decode(s).isSuccess shouldBe false
    }
  }
}
