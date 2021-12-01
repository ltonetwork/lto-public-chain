package com.ltonetwork.account

import com.ltonetwork.{TransactionGen, crypto}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatestplus.scalacheck.Checkers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import com.ltonetwork.utils.Base58
import com.ltonetwork.state.EitherExt2

class AccountSpecification extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Checkers with Matchers with TransactionGen {
  property("Account serialization round trip") {
    forAll(accountGen) { account: PrivateKeyAccount =>
      val bytes   = account.bytes.arr
      val address = Address.fromBytes(bytes).explicitGet()
      address.stringRepr shouldBe account.stringRepr
    }
  }

  property("Account.isValidAddress should return false for another address version") {
    forAll { (data: Array[Byte], AddressVersion2: Byte) =>
      val publicKeyHash   = crypto.secureHash(data).take(Address.HashLength)
      val withoutChecksum = AddressVersion2 +: AddressScheme.current.chainId +: publicKeyHash
      val addressVersion2 = Base58.encode(withoutChecksum ++ crypto.secureHash(withoutChecksum).take(Address.ChecksumLength))
      Address.fromString(addressVersion2).isRight shouldBe (AddressVersion2 == Address.AddressVersion)
    }
  }

  property("PublicKeyAccount should return Address as it's string representation") {
    forAll { bytes: Array[Byte] =>
      val a = PublicKeyAccount.apply(bytes)
      a.toString shouldBe a.toAddress.address
    }
  }
}
