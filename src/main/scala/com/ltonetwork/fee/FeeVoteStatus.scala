package com.ltonetwork.fee

case class FeeVoteStatus(vote: Byte, description: String, multiplier: Double) {
  def calc(fee: Long): Long = Math.round(fee * multiplier)
}

object FeeVoteStatus {
  object Decrease extends FeeVoteStatus(-1, "DECREASE", 1 / 1.1)
  object Remain extends FeeVoteStatus(0, "REMAIN", 1)
  object Increase extends FeeVoteStatus(1, "INCREASE", 1.1)

  // TODO Use Either and return Left for invalid description
  def apply(description: String): FeeVoteStatus = description.toUpperCase match {
    case Decrease.description => Decrease
    case Remain.description => Remain
    case Increase.description => Increase
    case _ => ???
  }

  def apply(vote: Byte): FeeVoteStatus = vote match {
    case v if v < 0 => Decrease
    case 0 => Remain
    case v if v > 0 => Increase
  }
}
