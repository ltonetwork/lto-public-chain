package com.ltonetwork.state

import cats.implicits._
import cats.kernel.Monoid
import com.ltonetwork.account.Address
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.transaction.smart.script.Script

case class Diff(transactions: Map[ByteStr, (Int, Transaction, Set[Address])],
                feeSponsors: Map[ByteStr, Address],
                portfolios: Map[Address, Portfolio],
                sponsoredBy: Map[Address, List[Address]], // multiple sponsors, first priority
                leaseState: Map[ByteStr, Boolean],
                scripts: Map[Address, Option[Script]],
                accountData: Map[Address, AccountDataInfo],
                burned: Long,
                certificate: Map[Address, Option[Array[Byte]]]) {

  lazy val accountTransactionIds: Map[Address, List[(Int, ByteStr)]] = {
    val map: List[(Address, Set[(Int, Byte, Long, ByteStr)])] = transactions.toList
      .flatMap { case (id, (h, tx, accs)) => accs.map(acc => acc -> Set((h, tx.builder.typeId, tx.timestamp, id))) }
    val groupedByAcc = map.foldLeft(Map.empty[Address, Set[(Int, Byte, Long, ByteStr)]]) {
      case (m, (acc, set)) =>
        m.combine(Map(acc -> set))
    }
    groupedByAcc
      .mapValues(l => l.toList.sortBy { case (h, _, t, _) => (-h, -t) }) // fresh head ([h=2, h=1, h=0])
      .mapValues(_.map({ case (_, typ, _, id) => (typ.toInt, id) }))
  }
}

object Diff {

  def apply(height: Int,
            tx: Transaction,
            portfolios: Map[Address, Portfolio] = Map.empty,
            sponsoredBy: Map[Address, List[Address]] = Map.empty,
            leaseState: Map[ByteStr, Boolean] = Map.empty,
            scripts: Map[Address, Option[Script]] = Map.empty,
            accountData: Map[Address, AccountDataInfo] = Map.empty,
            burned: Long = 0,
            certificate: Map[Address, Option[Array[Byte]]] = Map.empty): Diff =
    Diff(
      transactions = Map((tx.id(), (height, tx, portfolios.keys.toSet))),
      feeSponsors = Map.empty,
      portfolios = portfolios,
      sponsoredBy = sponsoredBy,
      leaseState = leaseState,
      scripts = scripts,
      accountData = accountData,
      burned = burned,
      certificate = certificate,
    )

  def fee(tx: Transaction, feeSponsor: Option[Address], portfolios: Map[Address, Portfolio]): Diff = {
    val feeSponsors = feeSponsor.map(address => Map((tx.id(), address))).getOrElse(Map.empty)
    new Diff(Map.empty, feeSponsors, portfolios, Map.empty, Map.empty, Map.empty, Map.empty, 0L, Map.empty)
  }

  val empty = new Diff(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, 0L, Map.empty)

  implicit val diffMonoid: Monoid[Diff] = new Monoid[Diff] {
    override def empty: Diff = Diff.empty

    override def combine(older: Diff, newer: Diff): Diff =
      Diff(
        transactions = older.transactions ++ newer.transactions,
        feeSponsors = older.feeSponsors ++ newer.feeSponsors,
        portfolios = older.portfolios.combine(newer.portfolios),
        sponsoredBy = older.sponsoredBy ++ newer.sponsoredBy, // whole list overriding
        leaseState = older.leaseState ++ newer.leaseState,
        scripts = older.scripts ++ newer.scripts,
        accountData = older.accountData.combine(newer.accountData),
        burned = older.burned + newer.burned,
        certificate = older.certificate ++ newer.certificate,
      )
  }
}
