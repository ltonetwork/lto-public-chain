package one.legalthings.it.sync.transactions

import com.typesafe.config.{Config, ConfigFactory}
import one.legalthings.it.api.SyncHttpApi._
import one.legalthings.it.transactions.BaseTransactionSuite
import org.scalatest.CancelAfterFailure
import play.api.libs.json.Json
import one.legalthings.it.sync._
import one.legalthings.transaction.lease.LeaseTransaction.Status.{Active, Canceled}

class LeaseStatusTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  import LeaseStatusTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  test("verification of leasing status") {
    val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee = minFee).id
    nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)
    val status = getStatus(createdLeaseTxId)
    status shouldBe Active

    val cancelLeaseTxId = sender.cancelLease(firstAddress, createdLeaseTxId, fee = minFee).id
    notMiner.waitForTransaction(cancelLeaseTxId)
    val status1 = getStatus(createdLeaseTxId)
    status1 shouldBe Canceled
    val sizeActiveLeases = sender.activeLeases(firstAddress).size
    sizeActiveLeases shouldBe 0

  }

  private def getStatus(txId: String): String = {
    val r = sender.get(s"/transactions/info/$txId")
    (Json.parse(r.getResponseBody) \ "status").as[String]

  }
}

object LeaseStatusTestSuite {
  private val blockGenerationOffest = "10000ms"
  import one.legalthings.it.NodeConfigs.Default

  private val minerConfig = ConfigFactory.parseString(s"""waves {
       |   miner{
       |      enable = yes
       |      minimal-block-generation-offset = $blockGenerationOffest
       |      quorum = 0
       |      micro-block-interval = 3s
       |      max-transactions-in-key-block = 0
       |   }
       |}
     """.stripMargin)

  private val notMinerConfig = ConfigFactory.parseString(s"""waves {
       |   miner.enable = no
       |   miner.minimal-block-generation-offset = $blockGenerationOffest
       |}
     """.stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1))
  )

}
