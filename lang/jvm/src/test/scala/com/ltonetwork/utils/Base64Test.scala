package com.ltonetwork.utils

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Base64Test extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  private val Base64Chars  = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/"
  private val IllegalChars = "!@#$%^&*()_-?/.,<>|\';:`~"

  val illegalGen: Gen[String] =
    for {
      length <- Gen.chooseNum(100, 1024)
      chars <- Gen
        .listOfN(length, Gen.oneOf(Base64Chars ++ IllegalChars))
        .filter(_.toSet.intersect(IllegalChars.toSet).nonEmpty)
    } yield chars.mkString

  property("handles empty sequences") {
    Base64.encode(Array.emptyByteArray) shouldBe ""
    Base64.decode("").get.length shouldBe 0
    Base64.decode("base64:").get.length shouldBe 0
  }

  property("decoding fails on illegal characters") {
    forAll(illegalGen) { s =>
      Base64.decode(s).isSuccess shouldBe false
    }
  }

  property("decoding fails on null") {
    Base64.decode(null).isSuccess shouldBe false
  }
}
