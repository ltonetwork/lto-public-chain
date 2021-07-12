package com.ltonetwork.state

import cats.kernel.Monoid

case class LeaseBalance(in: Long, out: Long)

object LeaseBalance {
  val empty: LeaseBalance = LeaseBalance(0, 0)

  implicit val m: Monoid[LeaseBalance] = new Monoid[LeaseBalance] {
    override def empty: LeaseBalance = LeaseBalance.empty

    override def combine(x: LeaseBalance, y: LeaseBalance): LeaseBalance =
      LeaseBalance(safeSum(x.in, y.in), safeSum(x.out, y.out))
  }
}