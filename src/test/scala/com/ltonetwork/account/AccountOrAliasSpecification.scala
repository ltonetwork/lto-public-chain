package com.ltonetwork.account

import com.ltonetwork.TransactionGen
import com.ltonetwork.state.EitherExt2
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class AccountOrAliasSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Account serialization round trip") {
    forAll(accountGen) { account: PrivateKeyAccount =>
      val bytes   = account.bytes.arr
      val address = Address.fromBytes(bytes).explicitGet()
      address.stringRepr shouldBe account.stringRepr
    }
  }

  property("AccountOrAlias serialization round trip") {
    forAll(accountOrAliasGen) { aoa: AddressOrAlias =>
      val bytes          = aoa.bytes.arr
      val addressOrAlias = AddressOrAlias.fromBytes(bytes, 0).explicitGet()
      addressOrAlias._1.stringRepr shouldBe aoa.stringRepr
    }
  }
}
