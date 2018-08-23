package one.legalthings.state.patch

import one.legalthings.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import one.legalthings.utils.ScorexLogging

object CancelAllLeases extends ScorexLogging {
  private def invertLeaseInfo(p: Portfolio) = Portfolio(0, LeaseBalance(-p.lease.in, -p.lease.out))

  def apply(s: Blockchain): Diff = {
    log.info("Collecting all active leases")
    val leasesToCancel = s.allActiveLeases.map(_.id() -> false).toMap
    leasesToCancel.foreach(id => log.info(s"Cancelling lease $id"))
    val portfolios = s.collectLposPortfolios { case (_, p) if p.lease != LeaseBalance.empty => invertLeaseInfo(p) }
    portfolios.keys.foreach(addr => log.info(s"Resetting lease balance for $addr"))
    log.info("Finished collecting all active leases")

    Diff.empty.copy(portfolios = portfolios, leaseState = leasesToCancel)
  }
}
