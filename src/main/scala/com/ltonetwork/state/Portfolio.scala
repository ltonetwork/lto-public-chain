package com.ltonetwork.state

import cats._
import com.ltonetwork.block.Block.Fraction

case class Portfolio(balance: Long, lease: LeaseBalance) {
  lazy val effectiveBalance: Long = safeSum(balance, lease.in) - lease.out
  lazy val spendableBalance: Long = balance - lease.out - lease.unbonding
  lazy val isEmpty: Boolean       = this == Portfolio.empty
}

object Portfolio {
  val empty: Portfolio                = Portfolio(0L, Monoid[LeaseBalance].empty)
  def apply(balance: Long): Portfolio = Portfolio(balance, LeaseBalance.empty)

  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  implicit val monoid: Monoid[Portfolio] = new Monoid[Portfolio] {
    override val empty: Portfolio = Portfolio.empty

    override def combine(older: Portfolio, newer: Portfolio): Portfolio =
      Portfolio(balance = safeSum(older.balance, newer.balance), lease = Monoid.combine(older.lease, newer.lease))
  }

  implicit class PortfolioExt(self: Portfolio) {

    def pessimistic: Portfolio = Portfolio(
      balance = Math.min(self.balance, 0),
      lease = LeaseBalance(
        in = 0,
        out = Math.max(self.lease.out, 0),
        unbonding = Math.max(self.lease.unbonding, 0)
      )
    )

    def multiply(f: Fraction): Portfolio =
      Portfolio(f(self.balance), LeaseBalance.empty)

    def minus(other: Portfolio): Portfolio =
      Portfolio(self.balance - other.balance, LeaseBalance.empty)
  }
}
