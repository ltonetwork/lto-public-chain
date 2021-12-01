package com.ltonetwork.network

import com.ltonetwork.TransactionGen
import io.netty.channel.embedded.EmbeddedChannel
import org.scalamock.scalatest.MockFactory
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class BrokenConnectionDetectorSpec extends AnyFreeSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks with TransactionGen {

  "should not close an active connection until the timeout" in {
    val handler = new BrokenConnectionDetector(400.millis)
    val ch      = new EmbeddedChannel(handler)

    ch.writeInbound("foo")
    Thread.sleep(200)
    ch.runPendingTasks()
    ch.isActive shouldBe true
  }

  "should not close a connection when messages are keep going" in {
    val handler = new BrokenConnectionDetector(100.millis)
    val ch      = new EmbeddedChannel(handler)

    (1 to 3).foreach { _ =>
      ch.writeInbound("bar")
      Thread.sleep(50)
      ch.runPendingTasks()
    }

    ch.isActive shouldBe true
  }

  "should close a broken connection" in {
    val handler = new BrokenConnectionDetector(200.millis)
    val ch      = new EmbeddedChannel(handler)

    ch.writeInbound("bar")
    Thread.sleep(250)
    ch.runPendingTasks()
    ch.isActive shouldBe false
  }

}
