package com.ltonetwork.http

import com.ltonetwork.api.http.ApiError
import com.ltonetwork.network._
import com.ltonetwork.transaction.{Transaction, ValidationError}
import com.ltonetwork.utx.UtxPool
import io.netty.channel.group.ChannelGroup

import scala.concurrent.Future

trait BroadcastRoute {
  def utx: UtxPool

  def allChannels: ChannelGroup

  import scala.concurrent.ExecutionContext.Implicits.global

  protected def doBroadcast(tx: Transaction): Future[Either[ApiError, Transaction]] = Future {
    val r = for {
      r <- utx.putIfNew(tx)
    } yield {
      val (added, _) = r
      if (added) allChannels.broadcastTx(tx, None)
      tx
    }

    r.left.map(ApiError.fromValidationError)
  }
}
