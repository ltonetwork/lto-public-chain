package com.ltonetwork.utils

trait Fraction {
  def apply(l: Long): Long;
}

object Fraction {
  private case class RoundDownFraction(dividend: Int, divider: Int) extends Fraction {
    def apply(l: Long): Long = l / divider * dividend
  }

  private case class RoundUpFraction(dividend: Int, divider: Int) extends Fraction {
    private val inv = RoundDownFraction(divider - dividend, divider)
    def apply(l: Long): Long = l - inv(l)
  }

  def roundDown(dividend: Int, divider: Int): Fraction = RoundDownFraction(dividend, divider)
  def roundUp(dividend: Int, divider: Int): Fraction = RoundUpFraction(dividend, divider)
}