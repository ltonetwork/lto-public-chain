package com.ltonetwork.fee

import com.ltonetwork.mining.MinerOptions
import com.ltonetwork.utils._
import com.ltonetwork.state._
import monix.execution.Scheduler.global

import scala.concurrent.duration._
import scala.io.Source
import java.io.File
import scala.util.Try

case class FeeVoteFileWatch(file: File, interval: FiniteDuration, minerOptions: MinerOptions) extends ScorexLogging {
  private def watch(): Unit = if (file.exists()) {
    val source = Source.fromFile(file)
    val status = source.getLines().next()
    source.close()
    vote(status)
  }

  private def vote(status: String): Unit = {
    val vote = FeeVoteStatus(status).explicitGet()

    if (minerOptions.feeVote != vote)
      log.info(s"Changing fee vote from ${minerOptions.feeVote} to ${vote}")

    minerOptions.feeVote = vote
  }

  file.delete() // Ignore existing vote file. It's persistent and can be old.

  global.scheduleAtFixedRate(0.seconds, interval) {
    Try(watch()).failed.map(e => log.error("Failed to process fee vote", e))
  }
}
