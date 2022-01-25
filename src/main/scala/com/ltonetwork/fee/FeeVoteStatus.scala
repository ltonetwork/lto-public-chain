package com.ltonetwork.fee

import com.ltonetwork.transaction.ValidationError.GenericError

case class FeeVoteStatus(vote: Byte, description: String, multiplier: Double) {
  def calc(fee: Long): Long = Math.round(fee * multiplier)
}

object FeeVoteStatus {
  object Decrease extends FeeVoteStatus(-1, "DECREASE", 1 / 1.1)
  object Remain extends FeeVoteStatus(0, "REMAIN", 1)
  object Increase extends FeeVoteStatus(1, "INCREASE", 1.1)

  def apply(description: String): Either[GenericError, FeeVoteStatus] = description.toUpperCase match {
    case Decrease.description => Right(Decrease)
    case Remain.description => Right(Remain)
    case Increase.description => Right(Increase)
    case _ => Left(GenericError(s"Invalid fee vote status '$description'"))
  }

  def apply(vote: Byte): Either[GenericError, FeeVoteStatus] = vote match {
    case v if v < 0 => Right(Decrease)
    case 0 => Right(Remain)
    case v if v > 0 => Right(Increase)
  }
}
