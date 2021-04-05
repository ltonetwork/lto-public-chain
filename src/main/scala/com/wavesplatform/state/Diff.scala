package com.wavesplatform.state

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.Script

case class LeaseBalance(in: Long, out: Long)

object LeaseBalance {
  val empty = LeaseBalance(0, 0)

  implicit val m: Monoid[LeaseBalance] = new Monoid[LeaseBalance] {
    override def empty: LeaseBalance = LeaseBalance.empty

    override def combine(x: LeaseBalance, y: LeaseBalance): LeaseBalance =
      LeaseBalance(safeSum(x.in, y.in), safeSum(x.out, y.out))
  }
}

case class VolumeAndFee(volume: Long, fee: Long)

object VolumeAndFee {
  val empty = VolumeAndFee(0, 0)

  implicit val m: Monoid[VolumeAndFee] = new Monoid[VolumeAndFee] {
    override def empty: VolumeAndFee = VolumeAndFee.empty

    override def combine(x: VolumeAndFee, y: VolumeAndFee): VolumeAndFee =
      VolumeAndFee(x.volume + y.volume, x.fee + y.fee)
  }
}

case class AccountDataInfo(data: Map[String, DataEntry[_]])

object AccountDataInfo {
  implicit val accountDataInfoMonoid: Monoid[AccountDataInfo] = new Monoid[AccountDataInfo] {
    override def empty: AccountDataInfo = AccountDataInfo(Map.empty)

    override def combine(x: AccountDataInfo, y: AccountDataInfo): AccountDataInfo = AccountDataInfo(x.data ++ y.data)
  }
}

object Sponsorship {
  val FeeUnit = 100000
}

case class Diff(transactions: Map[ByteStr, (Int, Transaction, Set[Address])],
                portfolios: Map[Address, Portfolio],
                sponsoredBy: Map[Address, List[Address]], // multiple sponsors, first priority
                leaseState: Map[ByteStr, Boolean],
                scripts: Map[Address, Option[Script]],
                accountData: Map[Address, AccountDataInfo]) {

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
            accountData: Map[Address, AccountDataInfo] = Map.empty): Diff =
    Diff(
      transactions = Map((tx.id(), (height, tx, portfolios.keys.toSet))),
      portfolios = portfolios,
      sponsoredBy = sponsoredBy,
      leaseState = leaseState,
      scripts = scripts,
      accountData = accountData
    )

  val empty = new Diff(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff.empty

    override def combine(older: Diff, newer: Diff): Diff =
      Diff(
        transactions = older.transactions ++ newer.transactions,
        portfolios = older.portfolios.combine(newer.portfolios),
        sponsoredBy = older.sponsoredBy ++ newer.sponsoredBy, // whole list overriding
        leaseState = older.leaseState ++ newer.leaseState,
        scripts = older.scripts ++ newer.scripts,
        accountData = older.accountData.combine(newer.accountData)
      )
  }
}
