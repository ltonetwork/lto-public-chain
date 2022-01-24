package com.ltonetwork.fee

case class FeeVoteStatus(vote: Byte, description: String, multiplier: Double) {
  def calc(fee: Long): Long = Math.round(fee * multiplier)
}

object FeeVoteStatus {
  object Decrease extends FeeVoteStatus(-1, "DECREASE", 1 / 1.1)
  object Unchanged extends FeeVoteStatus(0, "UNCHANGED", 1)
  object Increase extends FeeVoteStatus(1, "INCREASE", 1.1)

  def apply(description: String): FeeVoteStatus = description.toUpperCase match {
    case Decrease.description => Decrease
    case Unchanged.description => Unchanged
    case Increase.description => Increase
    case _ => ???
  }

  def apply(vote: Int): FeeVoteStatus = vote match {
    case v if v < 0 => Decrease
    case 0 => Unchanged
    case v if v > 0 => Increase
  }
}
