package com.ltonetwork.state.diffs.smart.predef

import com.ltonetwork.state._
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scodec.bits.ByteVector
import com.ltonetwork.account.Address

class AddressTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with TransactionGen with NoShrink {
  property("should calculate address from public key") {
    forAll(accountGen) { acc =>
      val script =
        s"""
           | let pk = base58'${ByteStr(acc.publicKey).base58}'
           | let address = addressFromPublicKey(pk)
           | address.bytes
        """.stripMargin
      runScript[ByteVector](script) shouldBe Right(ByteVector(Address.fromPublicKey(acc.publicKey, networkByte).bytes.arr))
    }
  }

  property("should calculate address from bytes") {
    forAll(accountGen) { acc =>
      val addressBytes = Address.fromPublicKey(acc.publicKey, networkByte).bytes
      val script =
        s"""
           | let addressString = "${addressBytes.base58}"
           | let maybeAddress = addressFromString(addressString)
           | let address = extract(maybeAddress)
           | address.bytes
        """.stripMargin
      runScript[ByteVector](script) shouldBe Right(ByteVector(Address.fromBytes(addressBytes.arr, networkByte).explicitGet().bytes.arr))
    }
  }

  property("should calculate address and return bytes without intermediate ref") {
    forAll(accountGen) { acc =>
      val addressBytes = Address.fromPublicKey(acc.publicKey, networkByte).bytes
      val script =
        s"""
           | let addressString = "${addressBytes.base58}"
           | let maybeAddress = addressFromString(addressString)
           | extract(maybeAddress).bytes
        """.stripMargin
      runScript[ByteVector](script) shouldBe Right(ByteVector(Address.fromBytes(addressBytes.arr, networkByte).explicitGet().bytes.arr))
    }
  }
}
