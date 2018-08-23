package one.legalthings.http

import one.legalthings.api.http.ApiError
import one.legalthings.network._
import one.legalthings.transaction.{Transaction, ValidationError}
import one.legalthings.utx.UtxPool
import io.netty.channel.group.ChannelGroup

import scala.concurrent.Future

trait BroadcastRoute {
  def utx: UtxPool

  def allChannels: ChannelGroup

  import scala.concurrent.ExecutionContext.Implicits.global

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] = Future {
    val r = for {
      tx <- v
      r  <- utx.putIfNew(tx)
    } yield {
      val (added, _) = r
      if (added) allChannels.broadcastTx(tx, None)
      tx
    }

    r.left.map(ApiError.fromValidationError)
  }
}
