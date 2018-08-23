package one.legalthings.state.diffs

import cats.implicits._
import one.legalthings.state._
import one.legalthings.account.Address
import one.legalthings.transaction.ValidationError
import one.legalthings.transaction.ValidationError.{GenericError, Validation}
import one.legalthings.transaction.transfer.MassTransferTransaction.ParsedTransfer
import one.legalthings.transaction.transfer._

object MassTransferTransactionDiff {

  def apply(blockchain: Blockchain, blockTime: Long, height: Int)(tx: MassTransferTransaction): Either[ValidationError, Diff] = {
    def parseTransfer(xfer: ParsedTransfer): Validation[(Map[Address, Portfolio], Long)] = {
      for {
        recipientAddr <- blockchain.resolveAlias(xfer.address)
        portfolio = Map(recipientAddr -> Portfolio(xfer.amount, LeaseBalance.empty))
      } yield (portfolio, xfer.amount)
    }
    val portfoliosEi = tx.transfers.traverse(parseTransfer)

    portfoliosEi.flatMap { list: List[(Map[Address, Portfolio], Long)] =>
      val sender   = Address.fromPublicKey(tx.sender.publicKey)
      val foldInit = (Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty)), 0L)
      val (recipientPortfolios, totalAmount) = list.fold(foldInit) { (u, v) =>
        (u._1 combine v._1, u._2 + v._2)
      }
      val completePortfolio = recipientPortfolios.combine(Map(sender -> Portfolio(-totalAmount, LeaseBalance.empty)))

      Right(Diff(height, tx, completePortfolio))
    }
  }
}
