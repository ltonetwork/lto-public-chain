package com.ltonetwork.it

import com.ltonetwork.http.DebugMessage
import com.ltonetwork.it.api.AsyncHttpApi._
import com.ltonetwork.utils.ScorexLogging
import org.scalatest.{Args, Status, Suite, SuiteMixin}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait ReportingTestName extends SuiteMixin with ScorexLogging {
  th: Suite with Nodes =>

  abstract override protected def runTest(testName: String, args: Args): Status = {
    print(s"Test '$testName' started")
    val r = super.runTest(testName, args)
    print(s"Test `$testName` finished with $r")
    r
  }

  private def print(text: String): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val formatted = s"---------- $text ----------"
    log.debug(formatted)
    try {
      Await.result(Future.traverse(nodes)(_.printDebugMessage(DebugMessage(formatted))), 10.seconds)
    } catch {
      case _: Throwable => ()
    }
  }
}
