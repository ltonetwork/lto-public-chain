package com.ltonetwork.state

import cats.kernel.Monoid
import com.ltonetwork.account.PrivateKeyAccount
import org.scalatest.{FunSuite, Matchers}

class DiffTest extends FunSuite with Matchers {
  test("combining diffs combines portfolios properly") {
    val address = PrivateKeyAccount.fromSeed("x").explicitGet().toAddress
    val d1      = Diff.empty.copy(portfolios = Map(address -> Portfolio(1, LeaseBalance.empty)))
    val d2      = Diff.empty.copy(portfolios = Map(address -> Portfolio(2, LeaseBalance.empty)))
    val r       = Monoid.combine(d1, d2)
    println(r)
  }
}
