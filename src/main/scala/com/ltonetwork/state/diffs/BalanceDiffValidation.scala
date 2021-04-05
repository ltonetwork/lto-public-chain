package com.ltonetwork.state.diffs

import cats.implicits._
import com.ltonetwork.metrics.Instrumented
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state.{Blockchain, ByteStr, Diff, LeaseBalance, Portfolio}
import com.ltonetwork.account.Address
import com.ltonetwork.utils.ScorexLogging
import com.ltonetwork.transaction.ValidationError.AccountBalanceError

import scala.util.{Left, Right}

object BalanceDiffValidation extends ScorexLogging with Instrumented {

  def apply(b: Blockchain, currentHeight: Int, fs: FunctionalitySettings)(d: Diff): Either[AccountBalanceError, Diff] = {

    val changedAccounts = d.portfolios.keySet

    val positiveBalanceErrors: Map[Address, String] = changedAccounts
      .flatMap(acc => {
        val portfolioDiff = d.portfolios(acc)
        val oldPortfolio  = b.portfolio(acc)

        val newPortfolio = oldPortfolio.combine(portfolioDiff)

        lazy val negativeBalance          = newPortfolio.balance < 0
        lazy val negativeEffectiveBalance = newPortfolio.effectiveBalance < 0
        lazy val leasedMoreThanOwn        = newPortfolio.balance < newPortfolio.lease.out

        val err = if (negativeBalance) {
          Some(s"negative lto balance: $acc, old: ${oldPortfolio.balance}, new: ${newPortfolio.balance}")
        } else if (negativeEffectiveBalance) {
          Some(s"negative effective balance: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(newPortfolio)}")
        } else if (leasedMoreThanOwn && oldPortfolio.lease.out == newPortfolio.lease.out) {
          Some(s"$acc trying to spend leased money")
        } else if (leasedMoreThanOwn) {
          Some(s"leased being more than own: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(newPortfolio)}")
        } else None
        err.map(acc -> _)
      })
      .toMap

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(AccountBalanceError(positiveBalanceErrors))
    }
  }

  private def leaseWavesInfo(p: Portfolio): (Long, LeaseBalance) = (p.balance, p.lease)

}
